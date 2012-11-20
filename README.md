# Tillich-Zemor Hashing

Implementation of Tillich-Zemor hashing in Clojure/Java.

To use it, build with Leiningen and include the following dependency in your Clojure project:

    [phlegmaticprogrammer/tillich_zemor_hash "1.0.0"]

It mainly provides the following four functions in the namespace *phlegmaticprogrammer.tillich_zemor_hash* :

- (hashbytes b)  
  Computes the hash value of a byte array b.
- (hashcomb & hashes)  
  Combines its arguments, all of which must be hash values, into a single hash value.
- (tobytes hashvalue)  
  Encodes a hash value as a byte array.
- (frombytes b)  
  Restores a hash value from a byte array.

The special thing about this hash function is that it is __composable__: 

Let U, V, W be three byte arrays such that W is the concatenation of U and W. Then 

    (= (hashbytes W) (hashcomb (hashbytes U) (hashbytes V)))

is true. This property also holds when concatenating more than two byte arrays. 

---


The basic idea of a Tillich-Zemor hash function is described in the paper:

- [**Hashing with SL 2**](http://dx.doi.org/10.1007/3-540-48658-5_5)    
  by _Jean-Pierre Tillich_ and _Gilles Zémor_   
  LNCS 839, Springer 1994  

The binary finite field arithmetic used is described in:

- **Guide to Elliptic Curve Cryptography**  
  by _Darrel Hankerson_, _Scott Vanstone_ and _Alfred Menezes_  
  [Chapter 2: Finite Field Arithmetic](http://www.springerlink.com/content/r51746598h0243u7/)  

The naive implementation for binary fields is written entirely in Clojure
and can be configured for any modulus. It uses the algorithms for addition 
and multiplication described in 2.3.1 and 2.3.2.

The default binary field implementation is written in Java, is much faster than the naive one,
and has the hard-wired modulus

    x^127 + x + 1

It uses an optimized version of 2.3.3 and 2.3.5 for multiplication/reduction.


  

