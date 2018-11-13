---
title: Checksum verification in Haskell
tags: haskell, mytardis
---

I made use of library called `cryptonite`.

Let's first discuss parsing the original checksum from text. `cryponite` notionally has something that will do just this: `digestFromByteString`. Now I am a novice when it comes to Haskell, but I quickly became aware that Strings in Haskell can be confusing... yay for all the beginners like me.

I started by calculating a checksum using my `shasum` command on my Mac:

```
❯ shasum ~/Desktop/boats.tif
0b7abd3eb68c98f0fda6b858c48dfff4040b44bc  /Users/keithschulze/Desktop/boats.tif
```

Let's put that in `ghci` and see if we can create a `Digest SHA1` out of it.

```
*Main Lib> import qualified Data.ByteString as BS
*Main Lib BS> let sha = "0b7abd3eb68c98f0fda6b858c48dfff4040b44bc"
*Main Lib BS> -- Convert String to ByteString
*Main Lib BS> BS.pack sha

<interactive>:4:9: error:
    • Couldn't match type ‘Char’ with ‘GHC.Word.Word8’
      Expected type: [GHC.Word.Word8]
        Actual type: [Char]
    • In the first argument of ‘BS.pack’, namely ‘sha’
      In the expression: BS.pack sha
      In an equation for ‘it’: it = BS.pack sha
```

String is really just a type alias for `[Char]` i.e., `newtype String = [Char]`. Turns out we need to use specific module that manipulates `ByteString` using `Char` operations and we can do this by importing `Data.ByteString.Char8`.

```
*Main Lib> import qualified Data.ByteString.Char8 as BS
*Main Lib BS> let sha = "0b7abd3eb68c98f0fda6b858c48dfff4040b44bc"
*Main Lib BS> -- Convert String to ByteString
*Main Lib BS> BS.pack sha
"0b7abd3eb68c98f0fda6b858c48dfff4040b44bc"
```

Now that we have a `ByteString` we can pass this to `digestFromByteString` function.

```
*Main Lib BS> import Crypto.Hash
*Main Lib BS Crypto.Hash> digestFromByteString (BS.pack sha) :: Maybe (Digest SHA1)
Nothing
```

Yikes, that didn't work as expected. Wikipedia gives us a hint of what might be going wrong here.

> In cryptography, SHA-1 (Secure Hash Algorithm 1) is a cryptographic hash function which takes an input and produces a 160-bit (20-byte) hash value known as a message digest - typically rendered as a hexadecimal number, 40 digits long.
> <footer class="blockquote-footer">Wikipedia</footer>

Our checksum string is actually a hexadecimal number and probably isn't decoded properly for `digestFromByteString`. Some searching reveals a package for [Hex encoding and decoding for `ByteStrings`](https://hackage.haskell.org/package/base16-bytestring).

```
*Main Lib BS Crypto.Hash> import Data.ByteString.Base16 as HEX
*Main Lib BS Crypto.Hash HEX> HEX.decode $ BS.pack sha
("\vz\189>\182\140\152\240\253\166\184X\196\141\255\244\EOT\vD\188","")
```

Putting it all together

```
*Main Lib BS Crypto.Hash HEX> digestFromByteString (fst $ HEX.decode (BS.pack sha)) :: Maybe (Digest SHA1)
Just 0b7abd3eb68c98f0fda6b858c48dfff4040b44bc
```

Yippee!
