{-# LANGUAGE FlexibleContexts #-}

module Linguistics.Syntax where
	import Data.JustParse
	import Data.JustParse.Char
	import Data.JustParse.Combinator

	import qualified Data.Map as Map

	import Control.Monad (fail)
	import Control.Applicative ((<$>), (<*>), pure)

	import Control.Monad.Reader hiding (fail)

	data LexicalCategory = Noun | Verb | Adjective | Adverb | Preposition | Determiner
							deriving (Eq, Show)

	type Word = String

	type LexMap = Map.Map Word [LexicalCategory]

	type LexParser u = Parser String

	data SyntaxTree = Word LexicalCategory Word
					| Phrase LexicalCategory SyntaxTree SyntaxTree
					| Bar LexicalCategory SyntaxTree SyntaxTree
					| SingleBar LexicalCategory Word
					deriving (Eq, Show)

	--parsePhrase :: LexParser () SyntaxTree -> LexMap -> Word -> Either ParseError SyntaxTree
	--parsePhrase p m s = runReader (runParserT (entirely p) () "" s) m

	entirely p = do
		x <- p
		eof
		return x

	phrase :: LexParser u SyntaxTree
	phrase = np <||> vp <||> pp

	np :: LexParser u SyntaxTree
	np = Phrase Noun <$> (Word Determiner <$> det) <*> noun'

	vp :: LexParser u SyntaxTree
	vp = Phrase Verb <$> np <*> verb'

	pp :: LexParser u SyntaxTree
	pp = Phrase Preposition <$> (Word Preposition <$> prep) <*> np <||> (Word Preposition <$> prep)

	noun' :: LexParser u SyntaxTree
	noun' = (SingleBar Noun <$> noun) <||> nounpps <||> (Bar Noun <$> (Word Adjective <$> adj) <*> noun')
		where
			nounpps = assemble <$> noun <*> (many1 pp)
				where
					assemble n ps = foldl (\x y -> Bar Noun x y) (SingleBar Noun n) ps

	verb' :: LexParser u SyntaxTree
	verb' = Bar Verb <$> (Word Adverb <$> adv) <*> verb' <||> verbadvs <||> SingleBar Verb <$> verb
		where
			verbadvs = assemble <$> verb <*> (many1 adv)
				where
					assemble v as = foldl (\x y -> Bar Verb x (Word Adverb y)) (SingleBar Verb v) as

	noun :: LexParser u Word
	noun = lexcat Noun

	verb :: LexParser u Word
	verb = lexcat Verb

	adj :: LexParser u Word
	adj = lexcat Adjective

	adv :: LexParser u Word
	adv = lexcat Adverb

	prep :: LexParser u Word
	prep = lexcat Preposition

	det :: LexParser u Word
	det = lexcat Determiner <||> pure ""

	lexcat :: LexicalCategory -> LexParser u Word
	lexcat cat = do
		w <- word
		let maybecats = Map.lookup w lexmap
		assert (maybecats /= Nothing)
		let cats = (\(Just x) -> x) maybecats
		assert (cat `elem` cats)
		return w

	word :: LexParser u Word
	word = firstOf <$> (many1 alphaNum) <*> (anull <$> optional (char ' '))

	firstOf = const
	anull = const ()

	lexmap = Map.fromList [("the", [Determiner]), ("cat", [Noun]), ("in", [Preposition]), ("hat", [Noun]), ("jump", [Verb]), ("quickly", [Adverb]), ("fat", [Adjective])]
