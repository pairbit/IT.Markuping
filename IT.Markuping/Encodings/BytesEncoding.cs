using System;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Encodings;

public class BytesEncoding //: IMarkupEncoding<byte>
{
    private readonly struct Tokens
    {
        public static readonly Tokens Utf16 = new(
            [(byte)'<', 0], [(byte)'>', 0], [(byte)'/', 0], [(byte)':', 0],
            [(byte)' ', 0], [(byte)'"', 0], [(byte)'\'', 0], [(byte)'=', 0]);

        public static readonly Tokens Utf16BE = new(
            [0, (byte)'<'], [0, (byte)'>'], [0, (byte)'/'], [0, (byte)':'],
            [0, (byte)' '], [0, (byte)'"'], [0, (byte)'\''], [0, (byte)'=']);

        public static readonly Tokens Utf32 = new(
            [(byte)'<', 0, 0, 0], [(byte)'>', 0, 0, 0], [(byte)'/', 0, 0, 0], [(byte)':', 0, 0, 0],
            [(byte)' ', 0, 0, 0], [(byte)'"', 0, 0, 0], [(byte)'\'', 0, 0, 0], [(byte)'=', 0, 0, 0]);

        public static readonly Tokens Utf32BE = new(
            [0, 0, 0, (byte)'<'], [0, 0, 0, (byte)'>'], [0, 0, 0, (byte)'/'], [0, 0, 0, (byte)':'],
            [0, 0, 0, (byte)' '], [0, 0, 0, (byte)'"'], [0, 0, 0, (byte)'\''], [0, 0, 0, (byte)'=']);

        //60,62,47,58,32,34,39,61
        public static readonly Tokens Utf8 = new(
            [(byte)'<'], [(byte)'>'], [(byte)'/'], [(byte)':'],
            [(byte)' '], [(byte)'"'], [(byte)'\''], [(byte)'=']);

        public static readonly Tokens EBCDIC = new(
            [76], [110], [97], [122], [64], [127], [125], [126]);

        public static readonly Tokens EBCDIC_Turkish = new(
            [76], [110], [97], [122], [64], [252], [125], [126]);

        internal readonly byte[] _lt;
        internal readonly byte[] _gt;
        internal readonly byte[] _slash;
        internal readonly byte[] _colon;
        internal readonly byte[] _space;//" "
        internal readonly byte[] _quot;//"
        internal readonly byte[] _apos;//'
        internal readonly byte[] _eq;//=

        public Tokens(byte[] lt, byte[] gt, byte[] slash, byte[] colon,
            byte[] space, byte[] quot, byte[] apos, byte[] eq)
        {
            _lt = lt;
            _gt = gt;
            _slash = slash;
            _colon = colon;
            _space = space;
            _quot = quot;
            _apos = apos;
            _eq = eq;
        }
    }

    #region Private Fields

    /// <summary>
    /// &lt;
    /// </summary>
    internal readonly byte[] _lt;

    /// <summary>
    /// &gt;
    /// </summary>
    internal readonly byte[] _gt;

    /// <summary>
    /// /
    /// </summary>
    internal readonly byte[] _slash;

    /// <summary>
    /// :
    /// </summary>
    internal readonly byte[] _colon;

    internal readonly byte[] _space;

    /// <summary>
    /// "
    /// </summary>
    internal readonly byte[] _quot;

    /// <summary>
    /// '
    /// </summary>
    internal readonly byte[] _apos;

    /// <summary>
    /// =
    /// </summary>
    internal readonly byte[] _eq;

    internal readonly byte[][] _otherSpaces;

    internal readonly int _minLength;

    #endregion Private Fields

    private BytesEncoding(Tokens tokens, byte[][]? otherSpaces = null)
        : this(tokens._lt, tokens._gt, tokens._slash, tokens._colon,
              tokens._space, tokens._quot, tokens._apos, tokens._eq,
              otherSpaces)
    {
    }

    public BytesEncoding(byte[] lt, byte[] gt, byte[] slash, byte[] colon,
        byte[] space, byte[] quot, byte[] apos, byte[] eq,
        byte[][]? otherSpaces = null)
    {
        _lt = lt;
        _gt = gt;
        _slash = slash;
        _colon = colon;
        _space = space;
        _quot = quot;
        _apos = apos;
        _eq = eq;

        _minLength = Math.Min(lt.Length, gt.Length);
        _minLength = Math.Min(_minLength, slash.Length);
        _minLength = Math.Min(_minLength, colon.Length);
        _minLength = Math.Min(_minLength, space.Length);
        _minLength = Math.Min(_minLength, quot.Length);
        _minLength = Math.Min(_minLength, apos.Length);
        _minLength = Math.Min(_minLength, eq.Length);

        if (otherSpaces != null)
        {
            _otherSpaces = otherSpaces;
            foreach (var otherSpace in otherSpaces)
            {
                _minLength = Math.Min(_minLength, otherSpace.Length);
            }
        }
        else
        {
            _otherSpaces = [];
        }
    }

    public static readonly BytesEncoding Utf16_Strict = new(Tokens.Utf16);
    public static readonly BytesEncoding Utf16BE_Strict = new(Tokens.Utf16BE);
    public static readonly BytesEncoding Utf32_Strict = new(Tokens.Utf32);
    public static readonly BytesEncoding Utf32BE_Strict = new(Tokens.Utf32BE);

    public static readonly BytesEncoding Utf16 = new(Tokens.Utf16, [[10, 0], [13, 0], [9, 0]]);
    public static readonly BytesEncoding Utf16BE = new(Tokens.Utf16BE, [[0, 10], [0, 13], [0, 9]]);
    public static readonly BytesEncoding Utf32 = new(Tokens.Utf32, [[10, 0, 0, 0], [13, 0, 0, 0], [9, 0, 0, 0]]);
    public static readonly BytesEncoding Utf32BE = new(Tokens.Utf32BE, [[0, 0, 0, 10], [0, 0, 0, 13], [0, 0, 0, 9]]);

    #region IMarkupEncoding

    public ReadOnlySpan<byte> Lt => _lt;

    public ReadOnlySpan<byte> Gt => _gt;

    public ReadOnlySpan<byte> Slash => _slash;

    public ReadOnlySpan<byte> Colon => _colon;

    public ReadOnlySpan<byte> Space => _space;

    public ReadOnlySpan<byte> Quot => _quot;

    public ReadOnlySpan<byte> Apos => _apos;

    public ReadOnlySpan<byte> Eq => _eq;

    #endregion IMarkupEncoding

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out BytesEncoding bytesEncoding)
    {
        if (codePage == 1200)
        {
            bytesEncoding = Utf16;
            return true;
        }
        if (codePage == 1201)
        {
            bytesEncoding = Utf16BE;
            return true;
        }
        if (codePage == 12000)
        {
            bytesEncoding = Utf32;
            return true;
        }
        if (codePage == 12001)
        {
            bytesEncoding = Utf32BE;
            return true;
        }
        bytesEncoding = null;
        return false;
    }

    public static bool TryGetStrict(int codePage, [MaybeNullWhen(false)] out BytesEncoding bytesEncoding)
    {
        if (codePage == 1200)
        {
            bytesEncoding = Utf16_Strict;
            return true;
        }
        if (codePage == 1201)
        {
            bytesEncoding = Utf16BE_Strict;
            return true;
        }
        if (codePage == 12000)
        {
            bytesEncoding = Utf32_Strict;
            return true;
        }
        if (codePage == 12001)
        {
            bytesEncoding = Utf32BE_Strict;
            return true;
        }
        bytesEncoding = null;
        return false;
    }
}