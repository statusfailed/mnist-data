# Data.MNIST

This package:

- `Data.Binary` instances for [MNIST data](http://yann.lecun.com/exdb/mnist/)
- Didn't use [mnist-idx][mnist-idx] because no support for pixels stored as
  `Word8`
- Works lazily
- Demo in `Main.hs` which prints the dataset and its labels.

[mnist-idx]: https://hackage.haskell.org/package/mnist-idx-0.1.2.8/docs/Data-IDX.html

# Example usage

In the package root,

    cabal repl
    :l Main
    :main ./data/images ./data/labels

Where `./data/images` and `./data/labels` contain (unzipped) MNIST images and
labels, respectively.
