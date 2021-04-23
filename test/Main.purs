module Test.Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Array (concat)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe, toNullable, null)
import Data.Tree (mkTree)
import Data.Tree.Zipper as Zipper
import Effect (Effect)
import Effect.Class (liftEffect)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.SignatureHelp (findArgumentIndex, parseSignature)
import LanguageServer.Protocol.Text (makeMinimalWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (TextDocument, createTextDocument)
import LanguageServer.Protocol.Types (ClientCapabilities, DocumentUri(..), LanguageId(..), Position(..), Range(..), TextDocumentEdit(..), TextEdit(..), WorkspaceEdit(..))
import Test.Unit (TestSuite, suite, suiteOnly, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

getEdit :: WorkspaceEdit -> Array TextEdit
getEdit (WorkspaceEdit {documentChanges}) = concat $ map go $ changes
  where
  go (TextDocumentEdit {edits}) = edits
  changes = fromMaybe [] $ toMaybe documentChanges

mkEdit :: Int -> Int -> String -> TextEdit
mkEdit n m t = TextEdit 
  { range: Range 
    { start: Position 
      { line: n, character: 0 }
    , end: Position
      { line: m, character: 0 }
    }
  , newText: t } 

capabilities :: ClientCapabilities
capabilities = 
  { workspace: toNullable $ Just 
    { applyEdit: toNullable $ Just true
      , workspaceEdit: toNullable $ Just $ 
        { documentChanges: toNullable $ Just true } 
    }
  , textDocument: null
  }
  
makeEdit :: String -> String -> Maybe WorkspaceEdit
makeEdit = makeMinimalWorkspaceEdit (Just capabilities) (DocumentUri "uri") 1.0

main :: Effect Unit
main = runTest do
  signatureHelpSuite
  suite "workspace edit" do
    test "update line" do
      let edit = makeEdit "A\nB\nC\n" "A\nXX\nC\n"
      Assert.equal (Just [ mkEdit 1 2 "XX\n"]) (getEdit <$> edit)
    test "insert line" do
      let edit = makeEdit "A\nC\n" "A\nB\nC\n"
      Assert.equal (Just [ mkEdit 1 1 "B\n"]) (getEdit <$> edit)
    test "insert line with more context" do
      let edit = makeEdit "A\n1\n2\n3\n4\n5\nC\n" "A\n1\n2\n3\nB\n4\n5\nC\n"
      Assert.equal (Just [ mkEdit 4 4 "B\n"]) (getEdit <$> edit)
    test "no difference" do
      let edit = makeEdit "A\nC\n" "A\nC\n"
      Assert.equal (Nothing) (getEdit <$> edit)
    test "first line changed" do
      let edit = makeEdit "A\nB\nC\n" "X\nB\nC\n"
      Assert.equal (Just [ mkEdit 0 1 "X\n"]) (getEdit <$> edit)
    test "last line changed" do
      let edit = makeEdit "A\nB\nC\n" "A\nB\nX\n"
      Assert.equal (Just [ mkEdit 2 3 "X\n"]) (getEdit <$> edit)

    test "CRLF" do
      let edit = makeEdit "A\r\nC\r\n" "A\r\nB\r\nC\r\n"
      Assert.equal (Just [ mkEdit 1 1 "B\n"]) (getEdit <$> edit)

    test "CRLF old-only" do
      -- If I have a CRLF file for some reason but IDE server gives me back LF
      let edit = makeEdit "A\r\nC\r\n" "A\nB\nC\n"
      Assert.equal (Just [ mkEdit 1 1 "B\n"]) (getEdit <$> edit)

    -- Type at point
    test "identifierAtPoint: identifies $" do
      let str = """$"""
      let result = identifierAtPoint str 0
      Assert.equal (result <#> _.word) (Just "$")
    test """identifierAtPoint: identifies <>""" do
      let str = """ 4 <> 3 """
      let result = identifierAtPoint str 3
      Assert.equal (result <#> _.word) (Just """<>""")
      Assert.equal (result <#> _.range) (Just { left: 3, right: 5 })
    test """identifierAtPoint: identifies /\""" do
      let str = """ 4 /\ 3 """
      let result = identifierAtPoint str 3
      Assert.equal (result <#> _.word) (Just """/\""")
      Assert.equal (result <#> _.range) (Just { left: 3, right: 5 })

dunno :: forall a. a
dunno = unsafeCoerce "dunno"

mkState :: forall a. a -> Zipper.Loc a
mkState x = Zipper.fromTree $ mkTree x Nil

signatureHelpSuite :: TestSuite
signatureHelpSuite = do
  suite "Signature help" do
    suiteOnly "Parses values" do
      test "gets correct position after entering a space" do 
        let testString = "log "
        textDocument <- mkTestDoc (exampleDoc <> testString) # liftEffect
        let position = Position { line: 6, character: 7}
        let state = mkState 
                    { functionName: "log"
                    , maxPosition: Position { line: 6, character: 6 }
                    , startPosition: Position { line: 6, character: 3 }
                    , parameters: NonEmptyArray.singleton "String"
                    , previousResponse: dunno
                    }
        actual <- runExceptT (findArgumentIndex position state testString)
        let expected = Right 0
        Assert.equal expected actual 

      test "gets correct position after entering a quote" do 
        let testString = "log \""
        textDocument <- mkTestDoc (exampleDoc <> testString) # liftEffect
        let position = Position { line: 6, character: 8}
        let state = mkState
                    { functionName: "log"
                    , maxPosition: Position { line: 6, character: 8 }
                    , startPosition: Position { line: 6, character: 3 }
                    , parameters: NonEmptyArray.singleton "String"
                    , previousResponse: dunno
                    }
        actual <- runExceptT (findArgumentIndex position state testString)
        let expected = Right 0
        Assert.equal expected actual 

      test "gets correct position after entering ' ('" do 
        let testString = "log ("
        textDocument <- mkTestDoc (exampleDoc <> testString) # liftEffect
        let position = Position { line: 6, character: 8}
        let state = mkState 
                    { functionName: "log"
                    , maxPosition: Position { line: 6, character: 7 }
                    , startPosition: Position { line: 6, character: 3 }
                    , parameters: NonEmptyArray.singleton "String"
                    , previousResponse: dunno
                    }
        actual <- runExceptT (findArgumentIndex position state testString)
        let expected = Right 0
        Assert.equal expected actual 

      test "gets correct position after entering '7 '" do 
        let testString = "add 7 "
        textDocument <- mkTestDoc (exampleDoc <> testString) # liftEffect
        let position = Position { line: 6, character: 9}
        let state = mkState 
                    { functionName: "add"
                    , maxPosition: Position { line: 6, character: 8 }
                    , startPosition: Position { line: 6, character: 3 }
                    , parameters: NonEmptyArray.singleton "String"
                    , previousResponse: dunno
                    }
        actual <- runExceptT (findArgumentIndex position state testString)
        let expected = Right 1
        Assert.equal expected actual 

      test "gets correct position after entering ' ()'" do 
        let testString = "log ()"
        textDocument <- mkTestDoc (exampleDoc <> testString) # liftEffect
        let position = Position { line: 6, character: 9 }
        let state = mkState 
                    { functionName: "log"
                    , maxPosition: Position { line: 6, character: 8 }
                    , startPosition: Position { line: 6, character: 3 }
                    , parameters: NonEmptyArray.singleton "String"
                    , previousResponse: dunno
                    }
        actual <- runExceptT (findArgumentIndex position state testString)
        let expected = Right 0
        Assert.equal expected actual 

      test "gets correct position after entering a newline" do 
        let testString = "log\n"
        textDocument <- mkTestDoc (exampleDoc <> testString) # liftEffect
        let position = Position { line: 6, character: 7 }
        let state = mkState 
                    { functionName: "log"
                    , maxPosition: Position { line: 6, character: 6 }
                    , startPosition: Position { line: 6, character: 3 }
                    , parameters: NonEmptyArray.singleton "String"
                    , previousResponse: dunno
                    }
        actual <- runExceptT (findArgumentIndex position state testString)
        let expected = Right 0
        Assert.equal expected actual 

    suite "Parses types" do
      test "monomorphic unary function" do 
        let actual = parseSignature "easy :: String -> Int" <#> _.parameters
        let expected = Just (pure "String")
        Assert.equal expected actual 
      test "monomorphic unary function that takes a Maybe" do 
        let actual = parseSignature "easy :: Maybe String -> Int" <#> _.parameters
        let expected = Just (pure "Maybe String")
        Assert.equal expected actual 
      test "function that takes a callback" do 
        let actual = parseSignature "easy :: (String -> Number) -> Int" <#> _.parameters
        let expected = NonEmptyArray.fromArray ["(String -> Number)"]
        Assert.equal expected actual 
      test "monomorphic binary function" do 
        let actual = parseSignature "easy :: String -> Int -> Int" <#> _.parameters
        let expected = NonEmptyArray.fromArray ["String", "Int"]
        Assert.equal expected actual 
      test "monomorphic record function" do 
        let actual = parseSignature "easy :: { name :: String } -> Int" <#> _.parameters
        let expected = NonEmptyArray.fromArray ["{ name :: String }"]
        Assert.equal expected actual 
      test "nested record function" do 
        let actual = parseSignature "easy :: { name :: { fore :: String, after :: String } } -> Int" <#> _.parameters
        let expected = NonEmptyArray.fromArray ["{ name :: { fore :: String, after :: String } }"]
        Assert.equal expected actual 
      test "parse type of log" do 
        let actual = parseSignature "log :: ∀ m. MonadEffect m ⇒ String → m Unit" <#> _.parameters
        let expected = NonEmptyArray.fromArray ["String"]
        Assert.equal expected actual 


mkTestDoc :: String -> Effect TextDocument
mkTestDoc = createTextDocument (DocumentUri "/TestExample.purs") (LanguageId "purescript") 1 

exampleDoc :: String
exampleDoc = """module TestExample where
import Prelude
import Data.Effect (Effect)

main :: Effect Unit
main = """