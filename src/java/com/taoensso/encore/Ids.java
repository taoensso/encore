/* Fast id generators */
package com.taoensso.encore;

import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.security.SecureRandom;

public class Ids {

    /* Common */

    // These arrays remain for the lifetime of each thread
    private static final ThreadLocal<char[]> CHARS_32 = new ThreadLocal<char[]>() { @Override protected char[] initialValue() { return new char[32]; } };
    private static final ThreadLocal<byte[]> BYTES_21 = new ThreadLocal<byte[]>() { @Override protected byte[] initialValue() { return new byte[21]; } };
    private static final ThreadLocal<byte[]> BYTES_16 = new ThreadLocal<byte[]>() { @Override protected byte[] initialValue() { return new byte[16]; } };

    private static final char[] localChars(int   max_size) { return (max_size <= 32) ? CHARS_32.get() : new char[max_size]; }
    private static final byte[] localBytes(int exact_size) {
        switch (exact_size) {
          case 21: return BYTES_21.get(); // Reusable local
          case 16: return BYTES_16.get(); // Reusable local
          default: return new byte[exact_size]; // Disposable
        }
    }

    // Can use java.util.concurrent.ThreadLocalRandom.current()) for insecure RNG
    public static final ThreadLocal<SecureRandom> SRNG        = ThreadLocal.withInitial(SecureRandom::new);
    public static final ThreadLocal<SecureRandom> SRNG_STRONG = ThreadLocal.withInitial(() -> {
            try { return SecureRandom.getInstanceStrong(); }
            catch (java.security.NoSuchAlgorithmException e) {
                throw new RuntimeException("Failed to initialize strong SecureRandom", e);
            }});

    /* Encodings */

    private static final char[] NANO_ENCODING = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_".toCharArray(); // 64 chars
    private static final char[]  HEX_ENCODING = hexEncodingChars();
    private static final char[] hexEncodingChars() {
        String HEX_ALPHABET = "0123456789abcdef"; // 16 chars
        char[] encoding = new char[512];
        for (int i = 0; i < 256; ++i) {
            encoding[i        ] = HEX_ALPHABET.charAt(i >>> 4);
            encoding[i | 0x100] = HEX_ALPHABET.charAt(i & 0xF);
        }
        return encoding;
    }

    /* NanoId (6 bit chars => 1 char per byte, 2 bits unused) */

    private static final int NANO_MASK = 63;
    public  static final String genNanoId(                   ) { return genNanoId(ThreadLocalRandom.current(), 21);  }
    public  static final String genNanoId(            int len) { return genNanoId(ThreadLocalRandom.current(), len); }
    public  static final String genNanoId(Random rng, int len) {
        char [] chars = localChars(len);
        byte [] bytes = localBytes(len);
        rng.nextBytes(bytes);
        for (int i = 0; i < len; ++i) {
            chars[i] = NANO_ENCODING[bytes[i] & NANO_MASK];
        }
        return new String(chars, 0, len);
    }

    /* HexId (4 bit chars => 2 chars per byte) */

    private static final void byteToBase16(byte value, char[] dest, int destOffset) {
        int b = value & 0xFF;
        dest[destOffset    ] = HEX_ENCODING[b        ];
        dest[destOffset + 1] = HEX_ENCODING[b | 0x100];
    }

    private static final void bytesToBase16(byte[] bytes, char[] dest, int length) {
        for (int i = 0; i < length; i++) {
            byteToBase16(bytes[i], dest, i * 2);
        }
    }

    private static final void intToBase16String(int value, char[] dest, int destOffset) {
        byteToBase16((byte) (value >> 24 & 0xFF), dest, destOffset);
        byteToBase16((byte) (value >> 16 & 0xFF), dest, destOffset + 2);
        byteToBase16((byte) (value >> 8  & 0xFF), dest, destOffset + 4);
        byteToBase16((byte) (value       & 0xFF), dest, destOffset + 6);
    }

    private static final void longToBase16String(long value, char[] dest, int destOffset) {
        byteToBase16((byte) (value >> 56 & 0xFFL), dest, destOffset);
        byteToBase16((byte) (value >> 48 & 0xFFL), dest, destOffset + 2);
        byteToBase16((byte) (value >> 40 & 0xFFL), dest, destOffset + 4);
        byteToBase16((byte) (value >> 32 & 0xFFL), dest, destOffset + 6);
        byteToBase16((byte) (value >> 24 & 0xFFL), dest, destOffset + 8);
        byteToBase16((byte) (value >> 16 & 0xFFL), dest, destOffset + 10);
        byteToBase16((byte) (value >> 8  & 0xFFL), dest, destOffset + 12);
        byteToBase16((byte) (value       & 0xFFL), dest, destOffset + 14);
    }

    private static final String toHexId8 (int  r) { char[] chars = localChars(32);  intToBase16String(r, chars, 0); return new String(chars, 0, 8);  }
    private static final String toHexId16(long r) { char[] chars = localChars(32); longToBase16String(r, chars, 0); return new String(chars, 0, 16); }
    private static final String toHexId32(long hi, long lo) {
        char[] chars = localChars(32);
        longToBase16String(hi, chars, 0);
        longToBase16String(lo, chars, 16);
        return new String(chars, 0, 32);
    }

    private static final String toHexId(byte[] bytes, int len) {
        char[] chars = localChars(len);
        bytesToBase16(bytes, chars, (len + 1) / 2); // Round up
        return new String(chars, 0, len);
    }

    private static final byte[] INVALID_HEX_BYTES_16 = new byte[16];
    private static final boolean validHexBytes(byte[] bytes) {
        // We disallow all-zero ids for compatibility with OpenTelemetry
        for (byte b : bytes) { if (b != 0) { return true; }}
        return false;
    }

    public static final String genHexId8 () { return genHexId8 (ThreadLocalRandom.current()); }
    public static final String genHexId16() { return genHexId16(ThreadLocalRandom.current()); }
    public static final String genHexId32() { return genHexId32(ThreadLocalRandom.current()); }

    public static final String genHexId8 (Random rng) { int  r; do { r = rng.nextInt();  } while (r == 0); return toHexId8 (r); }
    public static final String genHexId16(Random rng) { long r; do { r = rng.nextLong(); } while (r == 0); return toHexId16(r); }
    public static final String genHexId32(Random rng) {
        if (rng instanceof SecureRandom) {
            // Optimization: SRNG can get 16 bytes much faster than 2x longs (8+8 bytes)
            byte[] bytes = localBytes(16);
            do { rng.nextBytes(bytes); } while (Arrays.equals(bytes, INVALID_HEX_BYTES_16));
            return toHexId(bytes, 32);
        } else {
            long hi = rng.nextLong();
            long lo; do { lo = rng.nextLong(); } while (lo == 0);
            return toHexId32(hi, lo);
        }
    }

    public static final String genHexId(                   ) { return genHexId32(ThreadLocalRandom.current());    }
    public static final String genHexId(            int len) { return genHexId  (ThreadLocalRandom.current(), len); }
    public static final String genHexId(Random rng, int len) {
        switch (len) {
          case 32: return genHexId32(rng);
          case 16: return genHexId16(rng);
          case  8: return genHexId8 (rng);
        }

        byte[] bytes = localBytes((len + 1) / 2); // Round up
        do { rng.nextBytes(bytes); } while (!validHexBytes(bytes));
        return toHexId(bytes, len);
    }
}
