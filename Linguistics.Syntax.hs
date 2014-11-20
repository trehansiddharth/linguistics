{-# LANGUAGE FlexibleContexts #-}

module Linguistics.Syntax where
	import Prelude hiding (fail)

	import Data.JustParse
	import Data.JustParse.Char
	import Data.JustParse.Combinator

	import qualified Data.Map as Map

	import Control.Applicative ((<$>), (<*>), (<*), pure)

	import Control.Monad.Reader hiding (fail)

	data LexicalCategory = Noun | Verb | Adjective | Adverb | Preposition | Determiner
							deriving (Eq, Show)

	type Word = String

	data Preposition = P Word
		deriving (Eq, Show)
	data Noun = N Word
		deriving (Eq, Show)
	data Verb = V Word
		deriving (Eq, Show)
	data Determiner = Det Word
		deriving (Eq, Show)
	data Adjective = Adj Word
		deriving (Eq, Show)
	data Adverb = Adv Word
		deriving (Eq, Show)

	type LexMap = Map.Map Word [LexicalCategory]

	type LexParser = ReaderT LexMap (Parser String)

	data NP = NP Determiner N'
		deriving (Eq, Show)

	data N' = AdjN' Adjective N' | PrepN' N' PP | NounMin Noun | NounComp Noun PP
		deriving (Eq, Show)

	data VP = VP V'
		deriving (Eq, Show)

	data V' = NounV' V' NP | PrepV' V' PP | VerbMin Verb | VerbComp Verb NP
		deriving (Eq, Show)

	data PP = PP Preposition NP
		deriving (Eq, Show)

	data Sentence = Sentence NP VP
		deriving (Eq, Show)

	--parsePhrase :: LexParser () SyntaxTree -> LexMap -> Word -> Either ParseError SyntaxTree
	--parsePhrase p m s = runReader (runParserT (entirely p) () "" s) m

	entirely p = do
		x <- p
		lift eof
		return x

	{--phrase :: LexParser SyntaxTree
	phrase = np <||> vp <||> pp--}

	sentence :: LexParser Sentence
	sentence = Sentence <$> np <*> vp

	np :: LexParser NP
	np = NP <$> det <*> n'

	vp :: LexParser VP
	vp = VP <$> v'

	pp :: LexParser PP
	pp = PP <$> prep <*> np

	n' :: LexParser N'
	n' = choice' [AdjN' <$> adj <*> n', NounMin <$> noun, NounComp <$> noun <*> pp, foldl PrepN' <$> (NounMin <$> noun) <*> (many1' pp)]

	v' :: LexParser V'
	v' = choice' [VerbMin <$> verb, VerbComp <$> verb <*> np, NounV' <$> (VerbMin <$> verb) <*> np, foldl PrepV' <$> (VerbMin <$> verb) <*> (many1' pp)]

	noun :: LexParser Noun
	noun = N <$> lexcat Noun

	verb :: LexParser Verb
	verb = V <$> lexcat Verb

	adj :: LexParser Adjective
	adj = Adj <$> lexcat Adjective

	adv :: LexParser Adverb
	adv = Adv <$> lexcat Adverb

	prep :: LexParser Preposition
	prep = P <$> lexcat Preposition

	det :: LexParser Determiner
	det = Det <$> lexcat Determiner -- TODO: can also be null

	lexcat :: LexicalCategory -> LexParser Word
	lexcat cat = do
		w <- word
		maybecats <- asks (Map.lookup w)
		case maybecats of
			Nothing -> fail "" --"The word '" ++ w ++ "' is not in the dictionary."
			Just cats -> if cat `elem` cats
				then return w
				else fail "" --"There is no lexical category for the word '" ++ word ++ "' that is consistent with the rest of the phrase or sentence."

	fail x = lift (assert False) >> return x

	word :: LexParser Word
	word = lift $ firstOf <$> (many1 alphaNum) <*> (anull <$> optional (char ' '))

	firstOf = const
	anull = const ()

	choice' (x:xs) = (foldl branch') x xs

	branch' :: LexParser a -> LexParser a -> LexParser a
	branch' x y = ReaderT $ \r -> let x' = runReaderT x r in let y' = runReaderT y r in x' <||> y'

	many1' :: LexParser a -> LexParser [a]
	many1' x = ReaderT $ \r -> let x' = runReaderT x r in many1_ x'

	lexmap :: Map.Map String [LexicalCategory]
	lexmap = Map.fromList [("the", [Determiner]), ("cat", [Noun]), ("in", [Preposition]), ("hat", [Noun]), ("jump", [Verb]), ("quickly", [Adverb]), ("fat", [Adjective])]
