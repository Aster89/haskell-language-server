{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OrPatterns        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.CaseSplit
  ( caseSplitPluginCodeActionTitle
  , descriptor
  , Log
  ) where

-- TODO: **********************REMEMBER!!!**********************
-- make another pass with stylish-haskell
-- TODO: **********************REMEMBER!!!**********************
-- Review all haddock comments
import           Control.Applicative                   (ZipList (ZipList, getZipList))
import           Control.Arrow                         ((&&&))
import           Control.Lens                          ((^.), (^?))
import           Control.Monad                         (mzero, (>=>), when)
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.State.Strict            (MonadState (get, put),
                                                        State, evalState)
import           Control.Monad.Trans                   (lift)
import           Control.Monad.Trans.Except            (ExceptT)
import           Control.Monad.Trans.Maybe             (MaybeT, runMaybeT)
import           Data.Data                             (Data)
import           Data.Function                         (on, (&))
import           Data.Generics.Schemes                 (everywhereM)
import           Data.List.Extra                       (chunksOf, dropEnd,
                                                        takeEnd)
import           Data.List.NonEmpty                    (NonEmpty ((:|)), nonEmpty)
import qualified Data.List.NonEmpty                    as NE
import           Data.List.NonEmpty.Extra              ((|:), minimumBy1)
import           Data.Maybe                            (isJust, listToMaybe,
                                                        mapMaybe, maybeToList, isNothing)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Development.IDE                       (FileDiagnostic (fdStructuredMessage),
                                                        GetParsedModule (GetParsedModule),
                                                        GhcSessionDeps (GhcSessionDeps),
                                                        HscEnvEq (hscEnv),
                                                        IdeState (shakeExtras),
                                                        Pretty (pretty),
                                                        Recorder, WithPriority,
                                                        runAction,
                                                        srcSpanToRange)
import           Development.IDE.Core.FileStore        (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils      (activeDiagnosticsInRange,
                                                        runActionE, useE)
import           Development.IDE.GHC.Compat            (ConLike (RealDataCon),
                                                        HoleKind (HoleVar),
                                                        HsMatchContext (CaseAlt),
                                                        HscEnv (hsc_dflags), Id,
                                                        NamedThing (getName),
                                                        getLoc)
import           Development.IDE.GHC.Compat.Core       (AnnListItem,
                                                        EpAnnHsCase (EpAnnHsCase),
                                                        GrhsAnn (..),
                                                        HasSrcSpan,
                                                        HsLamVariant (LamCase),
                                                        HsMatchContext (LamAlt),
                                                        LocatedAn,
                                                        lann_trailing,
                                                        srcSpanStartCol,
                                                        srcSpanStartLine)
import qualified Development.IDE.GHC.Compat.Core       as Ext
import           Development.IDE.GHC.Compat.Error      (DsMessage (DsNonExhaustivePatterns),
                                                        _DsMessage,
                                                        msgEnvelopeErrorL)
import           Development.IDE.GHC.Compat.ExactPrint (d0, d1, exactPrint,
                                                        getEntryDP,
                                                        noAnnSrcSpanDP1,
                                                        setEntryDP)
import           Development.IDE.Types.Diagnostics     (FileDiagnostic (fdLspDiagnostic),
                                                        _SomeStructuredMessage)
import           GHC                                   (AnnList (AnnList),
                                                        AnnListBrackets (ListBraces),
                                                        DynFlags (extensions),
                                                        EpAnn (EpAnn),
                                                        EpToken (EpTok),
                                                        HasLoc (getHasLoc),
                                                        LMatch,
                                                        ParsedModule (pm_parsed_source),
                                                        realSrcSpan, ParsedSource)
import           GHC.Driver.DynFlags                   (OnOff (On))
import           GHC.Hs                                (DeltaPos (deltaColumn),
                                                        EpAnnLam (EpAnnLam),
                                                        GhcPs,
                                                        HsRecFields (HsRecFields),
                                                        XCase, XLam, deltaPos,
                                                        getDeltaLine,
                                                        unnamedHoleRdrName)
import           GHC.HsToCore.Pmc.Solver.Types         (Nabla (nabla_tm_st),
                                                        PmAltCon (..),
                                                        PmAltConApp (..),
                                                        TmState (ts_facts),
                                                        VarInfo (vi_pos))
import           GHC.Parser.Annotation                 (EpUniToken (EpUniTok),
                                                        IsUnicodeSyntax (NormalSyntax, UnicodeSyntax),
                                                        TrailingAnn (AddSemiAnn),
                                                        addTrailingAnnToA,
                                                        emptyComments,
                                                        noSrcSpanA)
import           GHC.Types.Name.Reader                 (nameRdrName)
import           GHC.Types.SrcLoc                      (GenLocated (L),
                                                        SrcSpan (RealSrcSpan),
                                                        combineSrcSpans)
import           GHC.Types.Unique.SDFM                 (lookupUSDFM)
import           Ide.Logger                            (logWith, Priority (Error))
import           Ide.Plugin.Error                      (getNormalizedFilePathE, PluginError)
import           Ide.PluginUtils                       (WithDeletions (IncludeDeletions),
                                                        diffText)
import           Ide.Types                             (PluginDescriptor (pluginHandlers),
                                                        PluginId,
                                                        PluginMethodHandler,
                                                        defaultPluginDescriptor,
                                                        mkPluginHandler,
                                                        pluginGetClientCapabilities, HandlerM, Config)
import           Language.Haskell.Syntax               (HsConDetails (PrefixCon, RecCon),
                                                        HsLocalBindsLR (EmptyLocalBinds),
                                                        LHsExpr,
                                                        MatchGroup (MG, mg_alts),
                                                        NoExtField (NoExtField),
                                                        Pat (..))
import           Language.Haskell.Syntax.Expr          (GRHS (GRHS),
                                                        GRHSs (GRHSs),
                                                        HsExpr (HsCase, HsHole, HsLam),
                                                        Match (..))
import qualified Language.LSP.Protocol.Lens            as L
import           Language.LSP.Protocol.Message         (Method (Method_TextDocumentCodeAction))
import qualified Language.LSP.Protocol.Message         as LSP
import           Language.LSP.Protocol.Types           (CodeAction (..),
                                                        CodeActionKind (CodeActionKind_QuickFix),
                                                        CodeActionParams (CodeActionParams, _range, _textDocument),
                                                        Range, isSubrangeOf,
                                                        type (|?) (InL, InR), WorkspaceEdit, VersionedTextDocumentIdentifier, NormalizedFilePath, Diagnostic, TextDocumentIdentifier)
import qualified Language.LSP.Protocol.Types           as Diag (Diagnostic (_range))
import           Type.Reflection                       (eqTypeRep,
                                                        type (:~~:) (HRefl),
                                                        typeOf, typeRep)
import Data.Semigroup (sconcat)
import Data.Foldable.Extra (firstJustM)
import Data.Traversable (for)


{- Note [Implementation strategy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  The present plugin achieves its target of inserting the missing patterns to a
  non-exhaustive @case@ (or @\case@) expression via the following strategy:

    1. retrieve the '[FileDiagnostic]' under the cursor,

    2. extract the 'Diagnostic' and the 'NonEmpty' list of missing
       'PmAltConApp' from the innermost "non-exhaustive patterns" diagnostic
       (several can be nested, in general),

    3. craft a 'CodeAction' and return it.

-}

data Log where
  LogASTUpdateError :: Log

instance Pretty Log where
  pretty LogASTUpdateError = "Error in updating the AST."

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Provides the split case code action")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction (suggestCaseSplitProvider recorder)
  }

suggestCaseSplitProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
suggestCaseSplitProvider recorder state _ CodeActionParams{ _textDocument, _range = cursor } = do

  nfp <- getNormalizedFilePathE $ _textDocument ^. L.uri

  fileDiags <- concat <$> activeDiagnosticsInRange (shakeExtras state) nfp cursor

  let diagAndMissingCtors = getInnermost $ extractDiagAndMissingCtors fileDiags

  codeAction <- firstJustM (makeCodeActions nfp) diagAndMissingCtors

  when (isNothing codeAction)
    $ logWith recorder Error LogASTUpdateError

  pure $ InL $ InR <$> maybeToList codeAction

  where
    makeCodeActions :: NormalizedFilePath -> (Diagnostic, MissingPatterns) -> ExceptT PluginError (HandlerM Config) (Maybe CodeAction)
    makeCodeActions nfp (diag, pmAltsConApps)
      -- TODO: update doc
      -- determine old and new text of the module
       = do arrowSyntax <- getArrowSyntax state nfp
            psOld <- getParsedSource state nfp

            for (graftMissingPatterns psOld pmAltsConApps cursor arrowSyntax)
                $ fmap (makeCodeAction diag) . makeWorkspaceEdit state _textDocument psOld

getParsedSource :: IdeState -> NormalizedFilePath -> ExceptT PluginError (HandlerM Config) ParsedSource
getParsedSource state nfp = pm_parsed_source <$> runActionE "CaseSplit.GetParsedModule"
                                                            state
                                                            (useE GetParsedModule nfp)

makeCodeAction :: Diagnostic -> WorkspaceEdit -> CodeAction
makeCodeAction diag edit
  = CodeAction { _title       = caseSplitPluginCodeActionTitle
               , _kind        = Just CodeActionKind_QuickFix
               , _diagnostics = Just [diag] -- TODO: is this really important? What if I just put Nothing?
               , _isPreferred = Nothing
               , _disabled    = Nothing
               , _edit        = Just edit
               , _command     = Nothing
               , _data_       = Nothing }

makeWorkspaceEdit :: IdeState -> TextDocumentIdentifier -> ParsedSource -> ParsedSource -> ExceptT PluginError (HandlerM Config) WorkspaceEdit
makeWorkspaceEdit state _textDocument psOld psNew
  = do verTxtDocId <- liftIO $ runAction "CaseSplit.GetVersionedTextDoc" state $ getVersionedTextDoc _textDocument
       makeEditText verTxtDocId psOld psNew

makeEditText :: VersionedTextDocumentIdentifier -> ParsedSource -> ParsedSource -> ExceptT PluginError (HandlerM Config) WorkspaceEdit
makeEditText verTxtDocId psOld psNew = do
  let old = T.pack $ exactPrint psOld
  let new = T.pack $ exactPrint psNew
  caps <- lift pluginGetClientCapabilities
  pure $ diffText caps (verTxtDocId, old) new IncludeDeletions

getArrowSyntax :: IdeState -> NormalizedFilePath -> ExceptT PluginError (HandlerM Config) IsUnicodeSyntax
getArrowSyntax state nfp = do
  (hsc_dflags . hscEnv -> dynFlags) <- runActionE "CaseSplit.GhcSessionDeps" state $ useE GhcSessionDeps nfp
  pure $ if On Ext.UnicodeSyntax `elem` extensions dynFlags
    then UnicodeSyntax
    else NormalSyntax

extractDiagAndMissingCtors :: [FileDiagnostic] -> [(Diagnostic, NonEmpty PmAltConApp)]
extractDiagAndMissingCtors = -- pair each file diag with its ds messages, if any
                   map (fdLspDiagnostic &&& (getDsMessage >=> getPmAltConApps >=> nonEmpty))
                   -- discard those with 'Nothing' as messages and unwrap
                   -- the surviving 'Just's
                   -- wrap back in the monad
                   -- extract the 'Diagnostic' and the pattern-match constructors for
                   -- each diag-and-message, only retaining those with some constructor
                   -- discard those with 'Nothing' as alternatives and
                   -- unwrap the surviving 'Just's
                .> (mapMaybe sequence :: [(a, Maybe b)] -> [(a, b)])
  where
    (.>) = flip (.)

    getDsMessage :: FileDiagnostic -> Maybe DsMessage
    getDsMessage d = fdStructuredMessage d ^? _SomeStructuredMessage . msgEnvelopeErrorL . _DsMessage

    getPmAltConApps :: DsMessage -> Maybe [PmAltConApp]
    getPmAltConApps =
      \case DsNonExhaustivePatterns CaseAlt _ _ [identifier] nablas -> nablasToPmAlts identifier nablas
            DsNonExhaustivePatterns (LamAlt LamCase) _ _ [identifier] nablas -> nablasToPmAlts identifier nablas
            _ -> Nothing

getInnermost :: [(Diagnostic, b)] -> Maybe (Diagnostic, b)
getInnermost [] = Nothing
getInnermost fileDiagAndDsMsg =
  -- TODO: update doc
  fileDiagAndDsMsg & nonEmpty
                   -- obtain the innermost diag-and-message
                   & fmap (minimumBy1 (ordSubrange `on` Diag._range . fst))

caseSplitPluginCodeActionTitle :: Text
caseSplitPluginCodeActionTitle = "Add placeholders for the first `-fmax-uncovered-patterns` missing patterns"

-- | Assign an 'Ordering' to two 'Range's @r1@ and @r2@ of which either is assumed to be subset of the other.
-- Will throw a runtime error if @r1@ is not a subrange of @r2@ or vice versa.
ordSubrange :: Range -> Range -> Ordering
ordSubrange r1 r2
  | r1 == r2 = EQ
  | r1 `isSubrangeOf` r2 = LT
  | r2 `isSubrangeOf` r1 = GT
  | otherwise = error "ordSubrange: ranges are not subranges of each other"

-- | Retrieve list of pattern match constructors
-- for the type identified by the given 'Id'.
--
-- Relevant information at https://simon.peytonjones.org/assets/pdfs/lower-your-guards.pdf
nablasToPmAlts :: Id -> [Nabla] -> Maybe [PmAltConApp]
nablasToPmAlts identifier nablas = fmap concat $ traverse go nablas
  where
    go = fmap vi_pos
       . flip lookupUSDFM identifier
       . ts_facts
       . nabla_tm_st

type MissingPatterns = NonEmpty PmAltConApp

-- | TODO: update doc
-- Given a 'ParsedModule' this function uses 'exactPrint' to produce the
-- 'Text's of said module before and after the 'MissingPatterns' are appended
-- to the existing ones in the innermost @case@ expression enclosing the
-- 'Range' of the cursor, using the arrow style passed as the last
-- 'IsUnicodeSyntax' argument.
graftMissingPatterns :: ParsedSource -> MissingPatterns -> Range -> IsUnicodeSyntax -> Maybe ParsedSource
graftMissingPatterns ps missingPs cursor arrowSyntax =
  -- We want to update exactly one node of the AST, the one that is
  -- associated to the innermost @case@ expression containing the cursor,
  -- therefore:
  runMaybeT (everywhereM go ps) -- we transform the 'ParsedSource' bottom-up
                                -- (allowing failure, incidentally),
    `evalState` False -- and we pass a 'Bool' through 'State' to bail
                      -- out after one update.

    where
      go :: forall a. Data a => a -> MaybeT (State Bool) a
      go node = do
          found <- get
          if | -- Proceed only if we haven't found & edited the node yet,
               not found
               -- only inspect nodes of the appropriate type,
             , Just HRefl <- typeOf node `eqTypeRep` typeRep @(HsExpr GhcPs)
               -- parse @case@-like expressions, and extract the 'SrcSpan' the
               -- whole expression occupies, as well as the indentation of the
               -- first alternative (see 'parseCaseLikeExpr' for more details),
             , Just (CaseLike {..}) <- parseCaseLikeExpr node
               -- make sure the cursor is somewhere in that span,
             , cursor `inSpan` _span
               -> do -- take note we've found the node,
                     put True
                     -- extract existing matches
                     let existingMatches = getMatchGroup _expr
                     -- make a match out of each missing pattern,
                     case traverse (makeMatch arrowSyntax) missingPs of
                        -- If something goes wrong, we communicate abortion,
                        Nothing             -> mzero
                        -- otherwise we continue.
                        Just missingMatches -> pure
                                             $ setMatches _expr
                                             $ appendMissingPats _layout existingMatches missingMatches
             -- Anything else, leave the node unchanged.
             | otherwise -> pure node

      -- | Predicate telling the given 'Range' falls within the given 'SrcSpan'.
      inSpan :: Range -> SrcSpan -> Bool
      inSpan range s = maybe False (range `isSubrangeOf`) (srcSpanToRange s)

data CaseLike = CaseLike { _expr :: CaseLikeExpr
                         , _span :: SrcSpan
                         , _layout :: MatchLayout
                         }

-- | While @HsExpr GhcPs@ can contain any expression, the following refined
-- type can only contain a @case@ or a @\case@ expression.
data CaseLikeExpr = Case       (XCase GhcPs) (LHsExpr GhcPs) (MatchGroup GhcPs (LHsExpr GhcPs))
                  | LambdaCase (XLam  GhcPs)                 (MatchGroup GhcPs (LHsExpr GhcPs))

-- | Get the 'MatchGroup' out of a 'CaseOrLamCase'.
getMatchGroup :: CaseLikeExpr -> MatchGroup GhcPs (LHsExpr GhcPs)
getMatchGroup (Case _ _ mg)     = mg
getMatchGroup (LambdaCase _ mg) = mg

-- | Parse an @HsCase _ _ mg@ or @HsLam _ LamCase mg@ out of a @HsExpr GhcPs@,
-- and return:
--
--      - the input `HsExpr GhcPs` information, but wrapped in the refined
--        type 'CaseOrLamCase',
--      - the 'SrcSpan' the parsed expression occupies,
--      - the information for correctly indenting the matches to be inserted
--        (see also 'MatchLayout').
parseCaseLikeExpr :: HsExpr GhcPs -> Maybe CaseLike

parseCaseLikeExpr (HsCase ext scrut matchGroup)
  | EpAnnHsCase (EpTok caseTok) (EpTok ofTok) <- ext
  , let caseSSpan = getHasLoc caseTok
        ofSSpan = getHasLoc ofTok
  , MG _ (L (EpAnn endTok _ _) _) <- matchGroup
  , let endSSpan = getHasLoc endTok
        span = caseExprSpan caseSSpan ofSSpan endSSpan
  = Just $ CaseLike { _expr = Case ext scrut matchGroup
                    , _span = span
                    , _layout = getMatchesLayout matchGroup
                    }

parseCaseLikeExpr (HsLam ext LamCase matchGroup)
  | EpAnnLam (EpTok backslashTok) (Just caseTok) <- ext
  , let backslashSSpan = getHasLoc backslashTok
        caseSSpan = getHasLoc caseTok
  , MG _ (L (EpAnn endTok _ _) _) <- matchGroup
  , let endSSpan = getHasLoc endTok
        span = caseExprSpan backslashSSpan caseSSpan endSSpan
  = Just $ CaseLike { _expr = LambdaCase ext matchGroup
                    , _span = span
                    , _layout = getMatchesLayout matchGroup
                    }

parseCaseLikeExpr _ = Nothing

-- | Isomorphic to @Maybe Matches@, this type encodes whether a @case@-like
-- expression has braces; in the positive case, it also records whether there's
-- pre-existing matches.
--
-- See also 'Matches'.
data MatchLayout = Braced Matches | NonBraced

-- | Isomorphic to @Maybe Int@, this type encodes whether there's pre-existing
-- matches in a @case@-like expression **with braces**, and - in the positive
-- case - what's the indentation of the first of them.
--
-- Note: it could also model the same concept for the non-braced case, but that's
-- not needed (see also 'MatchLayout').
data Matches = NoMatches | SomeMatches Int

-- | Given a 'MatchGroup', this function returns its 'MatchLayout'.
getMatchesLayout :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchLayout
getMatchesLayout (MG { mg_alts = L altsLoc existingMatches })
  = case (getOpeningBraceCol altsLoc, getStartCol <$> listToMaybe existingMatches) of
      (Nothing, _) -> NonBraced
      (_, Nothing) -> Braced NoMatches
      (Just openingBraceCol, Just fstExistingMatchCol)
        -> let indent = fstExistingMatchCol - openingBraceCol
           in Braced $ SomeMatches indent

-- | Given a @case@ or @\case@ expression wrapped in our refined
-- 'CaseOrLamCase' type and a 'MatchGroup', it creates an actual corresponding
-- @HsExpr GhcPs@ with that 'MatchGroup' in it.
setMatches :: CaseLikeExpr -> MatchGroup GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs
setMatches (Case x s _) mg     = HsCase x s mg
setMatches (LambdaCase x _) mg = HsLam x LamCase mg

-- | Given the 'SrcSpan' of the @case@ token, the @of@ token, and the end of
-- the alternatives, this function combines them to return a 'SrcSpan' that goes
-- from the @case@ token to the end of the whole @case@ expression.
caseExprSpan :: SrcSpan -> SrcSpan -> SrcSpan -> SrcSpan
caseExprSpan caseSSpan _ endSSpan@(RealSrcSpan _ _) = combineSrcSpans caseSSpan endSSpan
caseExprSpan caseSSpan ofSSpan _ = combineSrcSpans caseSSpan ofSSpan

-- | Given a 'MatchGroup' and a list of 'LMatch'es, this function inserts the
-- latter matches in the former group, trying to honor the existing layout,
-- returning the new 'MatchGroup' in the 'Maybe' monad to account for failure.
--
-- For the meaning of the first argument of type @Maybe Int@, see
-- 'getIndentation'.
--
-- Honoring the existing layout means two things:
--
--   1. producing valid code, which means:
--
--      - adding semicolons wherever they are needed, i.e.
--
--        - if matches are braced, for every matches,
--
--        - otherwise, for all but the last matches for groups of matches
--          that are not aligned vertically, e.g.
--
--            - matches shown on the same line, which this plugin can produce,
--
--            - matches shown on different lines but in a "staircase" way,
--              which this plugin never produces).
--
--      - using the correct indentation when matches are not braced (when
--        matches are braced, the code will stay valid irrespective of the
--        indentation of the alternatives).
--
--   2. such valid code tries to adhere to the existing layout, which means:
--
--      - don't alter position of existing matches nor of the opening @{@;
--
--      - when matches are not braced, we align the first match we insert
--        with the pre-existing previous match
--
--      - we have to make some arbitrary decision
--
--        - when matches are not braced and no previous match exists,
--          we indent by @indentation def@ with respect to whatever layout
--          context is the current one;
--
--        - as regards the number of matches to print per line, we inspect the
--          last group of matches appearing on one line, to determine how many
--          matches per line we insert.
--
--        - when matches are braced, we also align them vertically (it would
--          not be necessary, in principle).
--
--
-- Refer to test cases to see practical examples.
appendMissingPats :: MatchLayout
                  -> MatchGroup GhcPs (LHsExpr GhcPs)
                  -> NonEmpty (LMatch GhcPs (LHsExpr GhcPs))
                  -> MatchGroup GhcPs (LHsExpr GhcPs)
appendMissingPats matchLayout mg@(MG { mg_alts = L altsLoc existingMatches }) missingMatches
  = let -- Choose how many patterns per line we are emitting:
        chunkSize = case existingMatches of
                 [] -> 1 -- trivially 1 if there's no existing matches,
                      -- otherwise, set the size equal to the length
                      -- of the last group of @existingMatches@ that
                      -- are on the same line:
                 _ -> NE.length
                    $ NE.last
                    $ NE.groupBy1 startSameLine (NE.fromList existingMatches)

        -- Chunkify the matches to be inserted:
        missingGroup :| missingGroups = prettyChunksOf chunkSize missingMatches

        -- Detect if the list of alternatives is between @{@ and @}@:
        isBraced = isJust $ getOpeningBraceCol altsLoc

        -- Finally, lay out the missing matches:
        missingMatchesEP = -- indent the first group and the following ones (see discussion above)
                           mapFirst indentHead missingGroup :| map (mapFirst indentTail) missingGroups
                           -- add a semicolon to the end of each group only if the alternatives are braced
                         & (if isBraced then addSemicols else id)
                           -- put each group on its own line
                         & NE.map (mapFirst putOnNewLine)
                           -- concatenate the groups
                         & sconcat
                           -- turn into an ordinary list
                         & NE.toList
          where
            -- add semicolons:
            addSemicols = NE.zipWith ($)
                                      -- for each one-line group of matches,
                                     (replicate (length missingGroups)
                                                -- only to the last match of the group,
                                                (mapLast addSemiCol)
                                      -- except for the last group
                                      |: id)

            -- Indentation is complicated.
            --
            -- For a non-braced @case@-like expression, the first match **of the
            -- whole expression** (I mean, not the first match **to be inserted**)
            -- has some anchor that depends on the surrounding code, while the
            -- following matches all use their own predecessor as the anchor.
            --
            -- Otherwise (i.e. for a braced @case@-like expression), all matches
            -- including the first one have the same anchor that depends on the
            -- surrounding code.
            --
            -- Therefore, here's how we set the DeltaPos for the first and
            -- following matches:
            (setDPCol -> indentHead, setDPCol -> indentTail)
               = case matchLayout of
                   NonBraced | null existingMatches  -> (indentation def, 0)
                   NonBraced                         -> (0, 0)
                   Braced (SomeMatches indent)       -> (indent, indent)
                   Braced NoMatches                  -> let indent = indentation def
                                                        in (indent, indent)

        -- Only if there's braces do we need to make sure the last of the
        -- existing matches ends with @;@:
        existingMatchesEP = if isBraced
                               then dropEnd 1 existingMatches <> (addSemiCol <$> takeEnd 1 existingMatches)
                               else existingMatches

    in mg { mg_alts = L altsLoc (existingMatchesEP <> missingMatchesEP) }

-- | Accepts a @NonEmpty (LocatedAn AnnListItem a)@ and chunkifies it by the given 'size',
-- putting all matches of each chunk on the same line, leaving 1 space in between, and
-- keeping the code valid by adding semicolons to all but the last match of each chunk.
prettyChunksOf :: Int -> NonEmpty (LocatedAn AnnListItem a) -> NonEmpty (NonEmpty (LocatedAn AnnListItem a))
prettyChunksOf size allMatches = do
  -- For each chunk
  chunk <- chunksOf1 size allMatches
  pure $ fromZipList
       $ do -- of all the matches of chunk
            match       <- toZipList chunk
            -- from the second match onwards, they go the same line, one space apart
            putBeside   <- toZipList $ id :| repeat (setDP 0 1)
            -- all but the last match get a semicolon
            addSemicols <- toZipList $ replicate (length chunk - 1) addSemiCol |: id
            -- apply
            pure $ addSemicols $ putBeside match
  where
    toZipList = ZipList . NE.toList
    fromZipList = NE.fromList . getZipList

-- | Given a 'IsUnicodeSyntax', describing whether to use @->@ or @→@, and a
-- 'PmAltConApp', this function produces an 'LMatch' to be inserted in the list
-- of existing 'LMatch'es contained by a 'MatchGroup'.
--
-- The returned 'LMatch' is wrapped in 'Maybe' to account for failure, and it
-- is constructed in its entirety, by passing "default" values wherever
-- possible, except, obviously, for two:
--
--  - the constructor name,
--  - the arguments to the constructor, all rendered as individual underscores
--    when there's less than @maxUnderscores def@, or as a single @{}@ otherwise.
makeMatch :: IsUnicodeSyntax -> PmAltConApp -> Maybe (LMatch GhcPs (LHsExpr GhcPs))
makeMatch arrow pmAltConApp = makeLMatch <$> parseSimpleConMatch arrow pmAltConApp

parseSimpleConMatch :: IsUnicodeSyntax -> PmAltConApp -> Maybe SimpleConMatch
parseSimpleConMatch arrow PACA{ paca_con = PmAltConLike (RealDataCon dataCon)
                          , paca_ids
                          }
  = let locatedCon = L noSrcSpanA $ nameRdrName $ getName dataCon
        conPat = case length paca_ids of
                    -- for low number of arguments
                    n | n <= maxUnderscores def
                         -- create as many underscores as needed
                      -> ConPat { pat_con_ext = (Nothing, Nothing)
                                , pat_con = locatedCon
                                , pat_args = PrefixCon $ map (const $ L noAnnSrcSpanDP1 $ WildPat NoExtField) paca_ids
                                }
                         -- otherwise use braces.
                    _ -> ConPat { pat_con_ext = (Just (EpTok d1), Just (EpTok d0))
                                , pat_con = locatedCon
                                , pat_args = RecCon (HsRecFields NoExtField [] Nothing)
                                }
    in Just
     $ SimpleConMatch { _arrow = arrow
                      , _conPat = conPat }

parseSimpleConMatch _ _ = Nothing

makeLMatch :: SimpleConMatch -> LMatch GhcPs (LHsExpr GhcPs)
makeLMatch SimpleConMatch{..}
  = L noSrcSpanA $ Match { m_ext = NoExtField
                         , m_ctxt = CaseAlt
                         , m_pats = L noSrcSpanA [L noSrcSpanA _conPat]
                         , m_grhss = GRHSs emptyComments
                                           -- TODO: check whether ga_sep default choice is really not printing anything.
                                           (NE.singleton $ L noSrcSpanA $ GRHS (EpAnn noSrcSpanA
                                                                                      (GrhsAnn{ ga_vbar = Nothing
                                                                                              , ga_sep = Right $ EpUniTok d1 _arrow })
                                                                                      emptyComments) []
                                                         $ L noSrcSpanA $ HsHole $ HoleVar $ L noAnnSrcSpanDP1 $ unnamedHoleRdrName)
                                           (EmptyLocalBinds NoExtField)
                         }

data SimpleConMatch = SimpleConMatch { _arrow :: IsUnicodeSyntax
                                     , _conPat :: Pat GhcPs
                                     }

data Default = Default {
  -- | Max number of underscores to show for the constructor of an alternative.
  -- Beyond this, the record syntax with empty braces is used.
  maxUnderscores :: Int
  -- | Indentation used when there's no existing alternatives to refer to.
  -- Such indentation is with respect to the current layout context.
, indentation    :: Int
  -- TODO other things that we could store here are:
  --
  --    - the maximum number of alternatives on one line
  --    - whether or not to put the @;@ for the last alternative
}

def :: Default
def = Default { maxUnderscores = 3
              , indentation = 2 }

-- | Predicate telling if two located annotations are (actually, start) on the
-- same line.
startSameLine :: LocatedAn ann e -> LocatedAn ann e -> Bool
startSameLine = (==) `on` getStartLine

-- | Given an @EpAnn (AnnList a)@ return the starting column of
-- its opening brace, if any, otherwise 'Nothing'.
getOpeningBraceCol :: EpAnn (AnnList a) -> Maybe Int
getOpeningBraceCol (EpAnn _ (AnnList _ (ListBraces (EpTok col) _) _ _ _) _) = Just $ getStartCol $ getHasLoc col
getOpeningBraceCol _ = Nothing

-- | Get the starting column of an 'HasSrcSpan'.
getStartCol :: HasSrcSpan a => a -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getLoc

-- | Get the starting line of an 'HasSrcSpan'.
getStartLine :: HasSrcSpan a => a -> Int
getStartLine = srcSpanStartLine . realSrcSpan . getLoc

-- | Set the DeltaPos for the given annotation.
setDP :: Int -> Int -> LocatedAn t a -> LocatedAn t a
setDP deltaLine deltaColumn lann = setEntryDP lann $ deltaPos deltaLine deltaColumn

-- | Set the deltaColumn for the given annotation.
setDPCol :: Int -> LocatedAn t a -> LocatedAn t a
setDPCol deltaColumn lann = setEntryDP lann
                          $ (\d -> deltaPos (getDeltaLine d) deltaColumn)
                          $ getEntryDP lann

-- | Set the deltaLine for the given annotation.
setDPLine :: Int -> LocatedAn t a -> LocatedAn t a
setDPLine deltaLine lann = setEntryDP lann
                          $ (\d -> deltaPos deltaLine (deltaColumn d))
                          $ getEntryDP lann
-- | Useful helper.
putOnNewLine :: LocatedAn t a -> LocatedAn t a
putOnNewLine = setDPLine 1

-- | Add semicolon, unless one is already present.
addSemiCol :: LocatedAn AnnListItem a -> LocatedAn AnnListItem a
addSemiCol (L l@(EpAnn _ ls _) e)
  | none isSemiCol (lann_trailing ls)
  = L (addTrailingAnnToA (AddSemiAnn (EpTok d0)) emptyComments l) e
  where
    isSemiCol :: TrailingAnn -> Bool
    isSemiCol (AddSemiAnn _) = True
    isSemiCol _              = False
addSemiCol l = l

-- | Version of 'Data.List.Extra.chunksOf' (**not** to be confused with
-- 'Data.List.Split.chunksOf') for a 'NonEmpty' lists.
chunksOf1 :: Int -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksOf1 n xs
  | n >= 1
  , (b:before, after) <- NE.splitAt n xs
    = (b :| before) :| case after of
                         [] -> []
                         _  -> map NE.fromList $ chunksOf n after
  | otherwise = error "chunksOf1: the `Int` argument should be ≥ 1"

-- | Maps a function @f@ over the first element of a 'NonEmpty' list.
mapFirst :: (a -> a) -> NonEmpty a -> NonEmpty a
mapFirst f (a :| as) = f a :| as

-- | Maps a function @f@ over the last element of a 'NonEmpty' list.
mapLast :: (a -> a) -> NonEmpty a -> NonEmpty a
mapLast f (a :| []) = f a :| []
mapLast f (a :| as) = a :| mapLast' f as
  where
    mapLast' f as = init as ++ [f $ last as]

-- | Convenient negation of 'any'.
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p xs = not $ any p xs
