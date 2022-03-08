module Main (main) where

import Language.Joy.Rewrite
import Test.Hspec

main :: IO ()
main = do
  hspec $ do

    describe "empty param" $ do
      it "rewrites"
        $ (do
            let r0 = "[] [M] concat => [M]"
            let r1 = "[a L] [M] concat => [a N] :- [L] [M] concat => [N]"
            rewrite [r0, r1] "[] [1 2] concat"
          )
        `shouldBe` Right (tokenize "[1 2]")

    describe "single elem" $ do
      it "rewrites"
        $ (do
            let r0 = "[] [M] concat => [M]"
            let r1 = "[a L] [M] concat => [a N] :- [L] [M] concat => [N]"
            rewrite [r0, r1] "[1] [2 3] concat"
          )
        `shouldBe` Right (tokenize "[1 2 3]")
      it "rewrites multiple elems"
        $ (do
            let r0 = "[] [M] concat => [M]"
            let r1 = "[a L] [M] concat => [a N] :- [L] [M] concat => [N]"
            rewrite [r0, r1] "[1 2] [3 4] concat"
          )
        `shouldBe` Right (tokenize "[1 2 3 4]")
      it "rewrites ifte"
        $ (do
            let r0 = "a a = => true"
            let r1 = "L [I] [T] [E] ifte => L T :- L I => M true"
            rewrite [r0, r1] "1 1 [=] [5] [6] ifte"
          )
        `shouldBe` Right (tokenize "1 1 5")
      it "returns original if nothing to rewrite"
        $ (do
            let r0 = "a a = => true"
            let r1 = "L [I] [T] [E] ifte => L T :- L I => M true"
            rewrite [r0, r1] "1 2 3 4 5"
          )
        `shouldBe` Right (tokenize "1 2 3 4 5")
    it "substitutes given strings" $ do
      rewrite ["a b swap => b a"] "foo bar swap" `shouldBe` Right ["bar", "foo"]
      rewrite ["a b swap => b a"] "foo bar swap baz quux"
        `shouldBe` Right ["bar", "foo", "baz", "quux"]
      rewrite ["a b swap => b a"] "red green foo bar swap baz quux"
        `shouldBe` Right ["red", "green", "bar", "foo", "baz", "quux"]
      rewrite ["a b swap => b a"] "3 4 swap" `shouldBe` Right ["4", "3"]
      rewrite ["a b => [b] a"] "a b s"
        `shouldBe` Right ["[", "b", "]", "a", "s"]
      rewrite ["a b => b a"] "a b swap" `shouldBe` Right ["b", "a", "swap"]
      rewrite ["a b => b"] "a b s" `shouldBe` Right ["b", "s"]
      rewrite ["a b => b a b a"] "a b s"
        `shouldBe` Right ["b", "a", "b", "a", "s"]
      rewrite ["a [P] dip => P a"] "a [b c] dip"
        `shouldBe` Right ["b", "c", "a"]
      rewrite ["[ ] unstack => newstack"] "[] unstack"
        `shouldBe` Right ["newstack"]
      rewrite ["[a b L] second => b"] "1 [2 3 4 5] second"
        `shouldBe` Right ["1", "3"]
      rewrite ["[a L] 1 at => a"] "[2 3 4] 1 at" `shouldBe` Right ["2"]
      rewrite ["[] [L] concat => [L]"] "[ ] [1 2 3] concat"
        `shouldBe` Right ["[", "1", "2", "3", "]"]

    it "keeps trailing stuff" $ do
      rewrite
          [ "[] [M] concat => [M]"
          , "[a L] [M] concat => [a N] :- [L] [M] concat => [N]"
          ]
          "[1 2] [3 4] concat 5 6"
        `shouldBe` Right (tokenize "[1 2 3 4] 5 6")
    it "rewrites in the middle" $ do
      rewrite
          [ "[] [M] concat => [M]"
          , "[a L] [M] concat => [a N] :- [L] [M] concat => [N]"
          ]
          "1 2 [3 4] [5 6 7] concat 8 9"
        `shouldBe` Right (tokenize "1 2 [3 4 5 6 7] 8 9")
  pure ()
