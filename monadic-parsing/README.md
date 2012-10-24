# Monadic Parsing in Haskell (Maybe)

_18 Feb 2012_

["Monadic Parsing in Haskell"](http://www.cs.nott.ac.uk/~gmh/pearl.pdf) is an excellent tutorial by Erik Meijer and Graham Hutton on defining recursive descent parsers in Haskell.

Something I found interesting about the paper was the choice to represent the success of a parser as a single-element list of results, and the failure of a parser as an empty list of results. I like to use optional types (e.g. Haskell's `Maybe` or Scala's `Option`) to represent these all-or-nothing cases, and in this article I explore the switch to such pattern.

## A type for parsers

From the paper, we get a defined parser type and a single-character consuming parser:

```haskell
newtype Parser a = Parser (String -> [(a,String)])

item :: Parser Char
item  = Parser (\cs -> case cs of
                          ""     -> []
                          (c:cs) -> [(c,cs)])
```

Replacing the result list with a `Maybe` is straightforward:

```haskell
newtype Parser a = Parser (String -> Maybe (a,String))

item :: Parser Char
item  = Parser (\cs -> case cs of
                          ""     -> Nothing
                          (c:cs) -> Just (c,cs))
```

## A monad of parsers

Next, the `Parser` is made into a `Monad`, and things start to get tricky:

```haskell
instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a,cs') <- parse p cs])
```

The list comprehension used in the `(>>=)` function takes a bit of work to mentally parse. Let's break it down. Keep in mind the type of this function, which is `Parser a -> (a -> Parser b) -> Parser b`.

```haskell
instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a,cs') <- parse p cs])
```

* First, the parser `p` is deconstructed to extract a function of type `String -> [(a,String)]`
* Next, this function is applied to the string `cs` to get a tuple `(a,cs')` of type `(a,String)`

```haskell
instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a,cs') <- parse p cs])
```

* The function `f`, which has type `a -> Parser b`, is applied to the tuple's first element, `a`
* The resulting `Parser b` is deconstructed via `parse`
* The extracted `b`, of type `String -> [(b,String)]` is applied to the string `cs'`

```haskell
instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a,cs') <- parse p cs])
```

* Finally, the whole thing is concatenated into a list (which has length zero or one) of tuples, with type `[(b,String)]`

Now the fun part, switching from a list to a `Maybe` as defined above:

```haskell
instance Monad Parser where
  return a = Parser (\cs -> Just (a,cs))
  p >>= f  = Parser (\cs -> case parse p cs of
                              Nothing      -> Nothing
                              Just (a,cs') -> parse (f a) cs')
```

This is a bit easier to parse in my head, and it is more apparent to me both how the binding works and that the return type will be `Parser b`.

## Choice combinators

The rest of the changes are trivial, and in many cases no changes are needed at all.

```haskell
class Monad m => MonadZero m where
  zero :: m a

instance MonadZero Parser where
  zero   = Parser (\cs -> Nothing)

class MonadZero m => MonadPlus m where
  (++) :: m a -> m a -> m a

instance MonadPlus Parser where
  p ++ q = Parser (\cs -> case parse p cs of
                            Just (a,cs') -> Just (a,cs')
                            Nothing      -> parse q cs)
```

## Recursion combinators

```haskell
string       :: String -> Parser String
string ""     = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}
```
