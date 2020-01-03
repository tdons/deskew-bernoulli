Deskew
======

This program converts a bitstream generated by a Bernoulli process with unknown `p` into a bitstream generated by a Bernoulli process with `p = 0.5`.
In other words: the program removes (unknown) bias.

The program works in a streaming fashion, e.g. on a (theoretically) infinite bitstream and achieves the theoretically optimal result (if the state tree can grow indefinitely).
This contrasts e.g. [Von Neumanns well-known extractor](https://en.wikipedia.org/wiki/Randomness_extractor#Von_Neumann_extractor). 

This program is a partial implementation of a paper titled "Streaming Algorithms for Optimal Generation of Random Bits", authored by Hongchao Zhou and Jehoshua Bruck.

Examples
--------

Some examples

```
$ r --slave -e 'cat(rbinom(1000000, 1, .9999), sep="")' | ./Main.hs 
 input: (p = 0.999898)
output: (p = 0.5009940357852882)
01010111000000000111010110000111011001110100101111111111111111001101010101001101
00110001011011000111100010010000110100010110101000101001011110010100001101101001
11011111011100000010101110011101111100000001111110110100011111110100001001000001
11110000111110010001000100001100101011011111000010100101111111100000101011110010
00110000011011010110001101001111000110100111110001011111001110100010100000011100
01000011010010110111000110101010010110011000111100100111100110110010011001111000
00111010110000010000000011011011101100001101000101010101100010011100101000000000
11001011111000111001001001010000001001000011100001101110101100111101001010000111
01111111001111001001100000111001101001000011001110101100011100000101110010110111
01000000001010011000010100111010110011000011011111111111011001101010111000100010
10011011011001101110010001100011011010111011011000011100011011011010010111011110
11010111000110111111100110111111001010000010110001010000111101010000011000001101
0110001010010111011110110000010101001100110101

$ r --slave -e 'cat(rbinom(1000000, 1, .0001), sep="")' | ./Main.hs 
 input: (p = 9.0e-5)
output: (p = 0.507411630558723)
00011110001011101100000001011000111101110011101101000101110010010010000010110011
11000100111110101100011100100111000010110100100101101001111101110000010111001111
11110011110010111100100000111110101011100001000100101010000110001110001000100111
10101101101110001010101101001000110111101110000000110011001100101011010000110010
11000101101011011111101100100010110000001001101011110010011011010110101100100111
00101000000101011001001100101100001000011001111010010000011011111111010010000101
00001100011111010001111000011110101011011100101011000001101001110111100001110101
01111110010010111011101111001101110100111010101111011001101000111100110001101101
01000100010111110011101001010111100000111111001011001100101001001100111011101001
10010000110111000000110010111011101110111010001110101001000001011001000011101001
10101101110001000010110010111110001000101111001010101010001001011010110101000

$ r --slave -e 'cat(rbinom(100, 1, .25), sep="")' | ./Main.hs 
 input: (p = 0.33)
output: (p = 0.5272727272727272)
0001101001010111000110011010111110110101001101100001101
```
