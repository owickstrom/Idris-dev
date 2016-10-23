module Main

import Control.IOExcept
import Data.So

import Effects
import Effect.File
import Effect.StdIO
import Effect.Exception

%default total

data PackageType = Executable | Library

data GenerateError = FileGenError FileError String

Show GenerateError where
  show (FileGenError err path) = show err ++ ": " ++ path

isValidName : String -> Bool
isValidName s = length s > 0
                && (find (not . isAlphaNum) (unpack s) == Nothing)

Name : Type
Name = (s : String ** So (isValidName s))

asString : Name -> String
asString name = fst name

record Package where
  constructor MkPackage
  type : PackageType
  root : String
  name : Name
  slug : String
  moduleName : String

writeGeneratedFile : (path : String) -> (contents : String) -> Eff () [FILE (), EXCEPTION GenerateError]
writeGeneratedFile path contents =
  case !(writeFile path contents) of
    FError e => raise (FileGenError e path)
    Succes => pure ()

printPackageFile : Package -> Eff () [FILE (), EXCEPTION GenerateError]
printPackageFile pkg = do
  let fpath = root pkg ++ "/" ++ moduleName pkg ++ ".ipkg"
  let header = "package " ++ slug pkg ++ "\n\n"
               ++ "opts = \"\"\n"
  let contents =
    case type pkg of
      Executable => header
                    ++ "main = " ++ moduleName pkg ++ "\n"
                    ++ "executable = " ++ slug pkg ++ "\n"
      Library => header
                 ++ "modules = " ++ moduleName pkg ++ "\n"
  writeGeneratedFile fpath contents

printDefaultModule : Package -> Eff () [FILE (), EXCEPTION GenerateError]
printDefaultModule pkg = do
  let fpath = root pkg ++ "/" ++ moduleName pkg ++ ".idr"
  let contents =
    case type pkg of
      Executable => "module Main\n\n"
                    ++ "main : IO ()\n"
                    ++ "main = putStrLn \"I am " ++ moduleName pkg ++ ".\"\n"
      Library => "module " ++ moduleName pkg++ "\n\n"
                 ++ "-- I am " ++ moduleName pkg ++ ".\n"
  writeGeneratedFile fpath contents

printGitIgnore : Package -> Eff () [FILE (), EXCEPTION GenerateError]
printGitIgnore pkg = do
  let fpath = root pkg ++ "/.gitignore"
  let contents = "*.ibc\n"
                 ++ "/" ++ slug pkg ++ "\n"
  writeGeneratedFile fpath contents

getString : String -> Eff String [STDIO]
getString what = do
  putStr (what ++ ": ")
  pure (trim !getStr)

partial
getBool : String -> Eff Bool [STDIO]
getBool what = do
  s <- getString (what ++ " [y|n]")
  case toLower s of
    "y" => pure True
    "n" => pure False
    _ => do putStrLn "Please answer y or no."
            getBool what

partial
getName : String -> Eff Name [STDIO]
getName what = do
  s <- getString what
  case choose (isValidName s) of
    Left p => pure (s ** p)
    Right _ => getName what

partial
getPackageType : Eff PackageType [STDIO]
getPackageType = do
  s <- getString "Package Type [executable|library]"
  case s of
    "executable" => pure Executable
    "library" => pure Library
    _ => do putStrLn "Invalid package type, please try again."
            getPackageType

partial
queryPackage : Eff Package [STDIO]
queryPackage = do
  type <- getPackageType
  name <- getName "Name"
  root <- if !(getBool "Use current directory as root?")
          then pure "."
          else getString "Package root directory"

  let slug = toLower (asString name)
  let moduleName =
    case unpack (asString name) of
      (c::cs) => pack (toUpper c :: cs)

  pure (MkPackage type root name slug moduleName)

printSuccessMsg : Package -> Eff () [STDIO]
printSuccessMsg pkg =
  let typeStr = case (type pkg) of
                  Library => "library"
                  Executable => "executable"
  in putStrLn ("Generated " ++ typeStr ++ " " ++ fst (name pkg) ++ " in " ++ (root pkg))

partial
generatePackage : Eff () [FILE (), STDIO, EXCEPTION GenerateError]
generatePackage = do
  pkg <- queryPackage
  printPackageFile pkg
  printDefaultModule pkg
  printGitIgnore pkg
  putStrLn ""
  printSuccessMsg pkg

partial
main : IO ()
main = do
  Right () <- makeDirectory "/tmp/wtf" 0o775 | Left err => print err
  ioe_run (run generatePackage) Prelude.Interactive.print pure
