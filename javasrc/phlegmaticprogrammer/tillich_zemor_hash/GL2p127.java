package phlegmaticprogrammer.tillich_zemor_hash;

/**
 * Implements finite field arithmetic (addition and multiplication) for
 * GL(2^127). Calculations are done via polynoms with coefficients in GL(2)
 * modulo x^127 + x + 1.
 */
public final class GL2p127 {

    public final static class Polynom {
        public long hi;
        public long lo;
	public String toString() {
	    return "{lo:"+lo+", hi:"+hi+"}";
	}
	public boolean equals(Object o) {
	    Polynom p = (Polynom) o;
	    return lo == p.lo && hi == p.hi;
	}
    }

    public static Polynom make(long lo, long hi) {
        Polynom p = new Polynom();
        p.lo = lo;
        p.hi = (hi << 1) >>> 1;
        return p;
    }

    public static void add(Polynom a, Polynom b, Polynom result) {
        result.hi = a.hi ^ b.hi;
        result.lo = a.lo ^ b.lo;
    }

    public static long lo(Polynom a) {
        return a.lo;
    }

    public static long hi(Polynom a) {
        return a.hi;
    }

    public static void mul(Polynom a, Polynom b, Polynom result) {
        // multiplication
        long c3 = 0, c2 = 0, c1 = 0, c0 = 0;
        long b2 = 0, b1 = b.hi, b0 = b.lo;
        long alo = a.lo, ahi = a.hi;
        long lo, hi;
        for (int k = 0; k < 64; k++) {
            lo = alo & 1;
            hi = ahi & 1;
            alo >>= 1;
            ahi >>= 1;
            c0 ^= b0 * lo;
            c1 ^= (b1 * lo) ^ (b0 * hi);
            c2 ^= (b2 * lo) ^ (b1 * hi);
            c3 ^= b2 * hi;           
            b2 = (b2 << 1) | (b1 >>> 63);
            b1 = (b1 << 1) | (b0 >>> 63);
            b0 = b0 << 1;   
        }
        // reduction modulo x^127+x+1
        b1 = (c3 << 1) | (c2 >>> 63);
        b0 = (c2 << 1) | (c1 >>> 63);
        c1 ^= b1;
        c0 ^= b0;
        b1 = (b1 << 1) | (b0 >>> 63);
        b0 = b0 << 1;
        c1 ^= b1;
        c0 ^= b0;
        result.hi = (c1 << 1) >>> 1;
        result.lo = c0;
    }
    
}


// {lo:7206616528, hi:0}
// {lo:3100470661, hi:0}
// should result in  {lo:-831831288492576112, hi:0}
// but currenly results in:  {lo:5528679114088133264, hi:0}   