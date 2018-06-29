import           Test.Hspec
import           Hangman (State (..))
import qualified Hangman

main :: IO ()
main = hspec $
  describe "Hangman game" $
    context "with secret word 'GEHEIM' and 3 lifes" $ do
      let game = Hangman.init "GEHEIM" 3
      let guess = foldl (flip Hangman.guess) game
      context "and no input" $ do
        it "displays '_ _ _ _ _ _'" $
          Hangman.display game `shouldBe` "_ _ _ _ _ _"
        it "gamestate is Running" $
          Hangman.state game `shouldBe` Running
        it "3 lifes are remaining" $
          Hangman.lifes game `shouldBe` 3
      context "user guessed 'E'" $ do
        let game_with_E = guess "E"
        it "displays '_ E _ E _ _" $
          Hangman.display game_with_E `shouldBe` "_ E _ E _ _"
        it "gamestate is Running" $
          Hangman.state game_with_E `shouldBe` Running
        it "3 lifes are remaining" $
          Hangman.lifes game_with_E `shouldBe` 3
      context "user guessed 'E' and 'g'" $ do
        let game_with_Eg = guess "Eg"
        it "displays 'G E _ E _ _" $
          Hangman.display game_with_Eg `shouldBe` "G E _ E _ _"
        it "gamestate is Running" $
          Hangman.state game_with_Eg `shouldBe` Running
        it "3 lifes are remaining" $
          Hangman.lifes game_with_Eg `shouldBe` 3
      context "user guessed 'E', 'g', 'x'" $ do
        let game_with_Egx = guess "Egx"
        it "displays 'G E _ E _ _" $
          Hangman.display game_with_Egx `shouldBe` "G E _ E _ _"
        it "gamestate is Running" $
          Hangman.state game_with_Egx `shouldBe` Running
        it "2 lifes are remaining" $
          Hangman.lifes game_with_Egx `shouldBe` 2
      context "user guessed all chars correctly" $ do
        let game_with_allChars = guess "EGHIM"
        it "displays 'G E H E I M" $
          Hangman.display game_with_allChars `shouldBe` "G E H E I M"
        it "gamestate is Won" $
          Hangman.state game_with_allChars `shouldBe` Won
        it "3 lifes are remaining" $
          Hangman.lifes game_with_allChars `shouldBe` 3
      context "user lost all their lifes" $ do
        let game_with_noLifesLeft = guess "gXhXX"
        it "displays 'G _ H _ _ _" $
          Hangman.display game_with_noLifesLeft `shouldBe` "G _ H _ _ _"
        it "gamestate is Lost" $
          Hangman.state game_with_noLifesLeft `shouldBe` Lost
        it "0 lifes are remaining" $
          Hangman.lifes game_with_noLifesLeft `shouldBe` 0
      context "user keeps guessing successfully after they lost" $ do
        let game_with_keepGuessing = guess "eXgXhiXm"
        it "displays 'G E H E I _" $
          Hangman.display game_with_keepGuessing `shouldBe` "G E H E I _"
        it "gamestate is Lost" $
          Hangman.state game_with_keepGuessing `shouldBe` Lost
        it "0 lifes are remaining" $
          Hangman.lifes game_with_keepGuessing `shouldBe` 0
      context "user keeps guessing wrong after they lost" $ do
        let game_with_keepGuessingWrong = guess "eXgXhiXX"
        it "displays 'G E H E I _" $
          Hangman.display game_with_keepGuessingWrong `shouldBe` "G E H E I _"
        it "gamestate is Lost" $
          Hangman.state game_with_keepGuessingWrong `shouldBe` Lost
        it "0 lifes are remaining" $
          Hangman.lifes game_with_keepGuessingWrong `shouldBe` 0                              