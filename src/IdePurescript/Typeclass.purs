module IdePurescript.Typeclass where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (foldMap, intercalate)
import Data.Either (Either(..))
import Data.Foldable (surround)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.String (trim)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, readInt, readString)
import Foreign.Internal.Stringify (unsafeStringify)
import Foreign.Object as Object
import LanguageServer.IdePurescript.Assist (lineRange')
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util.CST (sourceRangeToRange)
import LanguageServer.Protocol.Console (log)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (applyEdit)
import LanguageServer.Protocol.Text (makeWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (getVersion)
import LanguageServer.Protocol.Types (DocumentStore, DocumentUri(..), Position(..), Range(..), Settings)
import LanguageServer.Protocol.Types as LSP
import LanguageServer.Protocol.Window (showInformation)
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Type(..))
import PureScript.CST.Types as CST

handleAddInstance ∷ DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
handleAddInstance docs _ (ServerState { pscIdePort, connection: maybeConnection, clientCapabilities, parseResults }) args = do
  case pscIdePort, maybeConnection, args of
    Just _port, Just connection, [ argUri, argLine, argChar ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        -> 
        doSomething connection uri line char
    _, _, _ -> do 
        liftEffect $ maybe (pure unit) (flip log "mail fatch") maybeConnection
        pure unit
  where 
  doSomething connection uri line character = do
    let maybeParseResult = parseResults # Object.lookup uri
    maybeDoc <- liftEffect $ getDocument docs (DocumentUri uri)
    case maybeParseResult, maybeDoc of
      Just { result: ParseSucceeded mod }, Just doc -> do
        version <- liftEffect $ getVersion doc
        let position = Position { line, character }
        case getDeclarationAtPoint position mod of
            Just nameEndLine@{ name, endLine: Additive endLine, endCharacter: Additive endCharacter, info } -> do
                liftEffect $ showInformation connection ("info" <> info)
                liftEffect $ showInformation connection ("nameEndLine" <> unsafeStringify nameEndLine)
                let 
                  lines = 
                    [ "derive instance newtype" <> name <> " :: Newtype " <> name <> " _"
                    , "derive newtype instance eq" <> name <> " :: Eq " <> name
                    , "derive newtype instance ord" <> name <> " :: Ord " <> name
                    ]
                liftEffect $ showInformation connection ("lines" <> unsafeStringify lines)
                let 
                  editPosition = Position { line: endLine, character: endCharacter }
                  editRange = Range { start: editPosition, end: editPosition }
                  edit = 
                    makeWorkspaceEdit clientCapabilities (DocumentUri uri) version 
                      editRange $ foldMap ("\n" <> _) lines 
                void $ applyEdit connection edit
            _ -> do liftEffect $ log connection "fail identifier"
                    pure unit
        pure unit
      _, _ -> mempty

getDeclarationAtPoint ∷ ∀ a. LSP.Position -> CST.Module a -> Maybe { name :: String, endLine :: Additive Int, endCharacter :: Additive Int, info :: String }
getDeclarationAtPoint pos =
  foldMapModule
    $ defaultMonoidalVisitor
        { onDecl =
          case _ of
            CST.DeclNewtype 
              { keyword: newtypeToken, name: (CST.Name {  name: CST.Proper name }) } _ _ 
                (TypeConstructor (CST.QualifiedName { token: constructorToken })) -> do 
                let Range { start: newtypeTokenStart } = sourceRangeToRange newtypeToken.range
                let Range { end: endPosition@(Position { character: endCharacter, line: endLine })} = sourceRangeToRange constructorToken.range
                if pos >= newtypeTokenStart && pos <= endPosition then 
                  Just { name, endLine: Additive endLine, endCharacter: Additive endCharacter, info: "TypeConstructor" }
                else Nothing 
            _ -> Nothing
            
        }
        
        
        -- TypeConstructor (QualifiedName Proper)

-- -> "--"
--   = -> ""="
-- derive -> "derive"
-- derive -> "derive"
-- derive -> "derive"
-- derive newtype instance ordX :: Ord X
-- instance showX :: Show X where
--   show = genericShow

-- Data.Eq (class Eq)
-- Data.Ord (class Ord)
-- Data.Functor (class Functor)
-- For things with kind Type -> Type
-- Data.Newtype (class Newtype)
-- For newtype declarations, I guess
-- Data.Generic.Rep (class Generic)
-- Once this is there it gives you all this jazz:
-- Data.Generic.Rep.Bounded
-- Data.Generic.Rep.Enum
-- Data.Generic.Rep.Eq
-- Data.Generic.Rep.HeytingAlgebra
-- Data.Generic.Rep.Monoid
-- Data.Generic.Rep.Ord
-- Data.Generic.Rep.Ring
-- Data.Generic.Rep.Semigroup
-- Data.Generic.Rep.Semiring
-- Data.Generic.Rep.Show
