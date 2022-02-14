# deriving-th

A proof of concept GHC plugin for Template Haskell based deriving.

Inspired By https://twitter.com/TechnoEmpress/status/1492411553894871042

## Usage

To use `deriving-th` with a class, all you have to do is create an instance of
the `DerivingTH.Class.DeriveTH` class. For example:

```haskell
class Foo a where
  foo :: a -> Bool

instance DeriveTH Foo where
  deriveTH name = [d| instance Foo $(pure $ ConT name) where foo _ = True |]
```

The plugin is then invoked with the `via template` deriving strategy. This is
used just as normal `-XDerivingVia` but with the word template replacing the via
type.

```haskell
data Bar = Bar
  deriving Foo via template
```

Or in combination with `-XStandaloneDeriving`:

```haskell
data Baz = Baz

deriving via template instance Foo Baz
```

You can see these examples in use in the `test/` folder.

## TH Caveats

All of the normal Template Haskell caveats apply:

- Cross-compilation issues
- `DeriveTH` instances must be defined in a separate module from where they are
  used
- Instances must be derived further in the module than the type that they are
  for and before where they are used

Note that splices for standard deriving clauses are inserted directly after the
data type definition they are a part of in the same order that they appear in
the source. If greater flexibility in the location of splices is needed
`-XStandaloneDeriving` can be used.

## Implied Extensions

This plugin automatically enables the following extensions.

- `-XDerivingVia` for syntax
- `-XTemplateHaskell` for splices
- `-XTemplateHaskellQuotes` for name quotes

Extensions that are normally implied by these are not enabled automatically.

## Advanced Usage

The `DeriveTH` class is much more flexible than the standard use case
requires. The `deriveTH` method gives an arbitrary `Q [Dec]` so you can (ab)use
it to generate more than one instance or even declarations other than
instances. Additionally, the parameter of `DeriveTH` is fully polymorphic so you
can use it with classes with any kind or even non-classes. This adaptability may
be useful if you have mutually dependent instances or are wrapping some already
existing functions.

## Supported GHC Versions

At the moment GHC versions 8.10.1 though 9.2.1 are supported. If you need
another version, let me know.
