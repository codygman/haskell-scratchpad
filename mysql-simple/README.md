# Databases in Haskell with mysql-simple

*November 16, 2013*

Working with MySQL from Haskell is easy with the [mysql-simple](http://hackage.haskell.org/package/mysql-simple) library. To get started, install it with cabal:

```bash
$ cabal install mysql-simple
```

Out of the box, mysql-simple gives us several functions to issue queries:

```haskell
query :: (QueryParams q, QueryResults r) => Connection -> Query -> q -> IO [r]
query_ :: QueryResults r => Connection -> Query -> IO [r]
execute :: QueryParams q => Connection -> Query -> q -> IO Int64Source
execute_ :: Connection -> Query -> IO Int64Source
```

The `query` and `query_` functions are used to execute SQL queries that return results, such as `SELECT`, using `query` for those that take arguments, and `query_` for those that do not.

The `execute` and `execute_` functions are used to execute SQL queries that do not return results, such as `DROP`, `CREATE`, `INSERT`, and `UPDATE`, similarly using `execute` for those that take arguments, and `execute_` for those that do not.

In practice, it is convenient to abstract these functions from their `Connection` arguments:

```haskell
type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64
```

This way, we can compose several `SqlQuery` and `SqlCommand` functions together before we have a `Connection` available, building up a series of SQL queries to be run at a later time with just a single connection.

We can create `SqlQuery` and `SqlCommand` values using functions that delegate to mysql-simple's `query` and `execute`:

```haskell
sqlQuery :: (QueryParams q, QueryResults r) => Query -> q -> Connection -> IO [r]
sqlQuery q vs conn = query conn q vs

sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

sqlCmd :: QueryParams q => Query -> q -> Connection -> IO Int64
sqlCmd q vs conn = execute conn q vs

sqlCmd_ :: Query -> Connection -> IO Int64
sqlCmd_ q conn = execute_ conn q
```

There's nothing particularly interesting about these, since all we're doing here is rearranging the arguments, but they will come in handy later.

Before we can issue any queries, we need a dabase.  Let's set up a test database to play with:

```bash
$ mysql -uroot -p
Enter password: 
mysql> create database test;
mysql> grant all on test.* to test@localhost identified by 'test';
```

We configure a connection to this database with a mysql-simple `ConnectInfo` value:

```haskell
connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost = "localhost",
                            connectPort = 3306,
                            connectUser = "test",
                        connectPassword = "test",
                        connectDatabase = "test",
                         connectOptions = [],
                            connectPath = "",
                             connectSSL = Nothing }
```

Next we define our own `User` data type, plus a way to create it from a mysql-simple `QueryResults` value:

```haskell
data User = User { name :: String, age :: Int } deriving Show

instance QueryResults User where
  convertResults [fa,fb] [va,vb] = User { name = a, age = b }
    where a = convert fa va
          b = convert fb vb
  convertResults fs vs  = convertError fs vs 2
```

Finally, we can start defining some database query functions:

```haskell
clean :: SqlCommand
clean = sqlCmd_ "drop table if exists users"

create :: SqlCommand
create = sqlCmd_ "create table if not exists users (name text, age int)"

insert :: String -> Int -> SqlCommand
insert name age = sqlCmd "insert into users (name, age) values (?, ?)" (name,age)

select :: SqlQuery [User]
select = sqlQuery_ "select name, age from users"

user :: (String,Int) -> User
user (name,age) = User { name = name, age = age }
```

For a simple demo, we string together our query functions, run the resulting `SqlQuery`, and print the results:

```haskell
demo :: SqlQuery [User]
demo = clean >> create >> insert "Alice" 26 >> insert "Bob" 24 >> select

main :: IO ()
main = do
  conn  <- connect connectInfo
  users <- demo conn
  _     <- putStrLn $ show users
  return ()
```

The output of `main` shows two `User` values:

```bash
$ runhaskell MySQLExample.hs 
[User {name = "Alice", age = 26},User {name = "Bob", age = 24}]
```
