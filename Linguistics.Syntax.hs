{-# LANGUAGE FlexibleContexts #-}

module Linguistics.Syntax where
	import Text.Parsec
	import Text.Parsec.String

	import qualified Data.Map as Map

	import Control.Monad (fail)
	import Control.Applicative ((<$>), (<*>), pure)

	import Control.Monad.Reader hiding (fail)

	data LexicalCategory = Noun | Verb | Adjective | Adverb | Preposition | Determiner
							deriving (Eq, Show)

	type Word = String

	type LexMap = Map.Map Word [LexicalCategory]

	type LexParser u = ParsecT String u (Reader LexMap)

	data SyntaxTree = Word LexicalCategory Word
					| Phrase LexicalCategory SyntaxTree SyntaxTree
					| Bar LexicalCategory SyntaxTree SyntaxTree
					| SingleBar LexicalCategory Word
					deriving (Eq, Show)

	parsePhrase :: LexParser () SyntaxTree -> LexMap -> Word -> Either ParseError SyntaxTree
	parsePhrase p m s = runReader (runParserT (entirely p) () "" s) m

	entirely p = do
		x <- p
		eof
		return x

	np :: LexParser u SyntaxTree
	np = Phrase Noun <$> (Word Determiner <$> det) <*> noun'

	vp :: LexParser u SyntaxTree
	vp = Phrase Verb <$> np <*> verb'

	pp :: LexParser u SyntaxTree
	pp = try (Phrase Preposition <$> (Word Preposition <$> prep) <*> np) <|> (Word Preposition <$> prep)

	noun' :: LexParser u SyntaxTree
	noun' = try (Bar Noun <$> (Word Adjective <$> adj) <*> noun') <|> try nounpps <|> SingleBar Noun <$> noun
		where
			nounpps = assemble <$> noun <*> (many1 pp)
				where
					assemble n ps = foldl (\x y -> Bar Noun x y) (SingleBar Noun n) ps

	verb' :: LexParser u SyntaxTree
	verb' = try (Bar Verb <$> (Word Adverb <$> adv) <*> verb') <|> try verbadvs <|> SingleBar Verb <$> verb
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
	det = lexcat Determiner

	lexcat :: LexicalCategory -> LexParser u Word
	lexcat cat = do
		w <- word
		maybecats <- lift $ asks (Map.lookup w)
		case maybecats of
			Nothing -> fail $ "The word '" ++ w ++ "' is not in the lexmap"
			Just cats -> if cat `elem` cats
				then return w
				else fail $ "The word '" ++  w ++ "' does not fit any of the expected lexical categories, e.g., '" ++ (show cat) ++ "'"

	word :: LexParser u Word
	word = firstOf <$> (many alphaNum) <*> (anull <$> try space <|> anull <$> eof)

	firstOf = const
	anull = const ()
