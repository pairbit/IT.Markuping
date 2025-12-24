using IT.Markuping.Encodings;
using System;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public class TagFinderByte : TagFinder<byte>
{
    private readonly bool[]? _otherSpaces;
    //private readonly ByteEncoding.Tokens _tokens;

    public static readonly TagFinderByte Utf8 = new(ByteEncoding.Utf8);
    public static readonly TagFinderByte Europa = new(ByteEncoding.Europa);
    public static readonly TagFinderByte EBCDIC = new(ByteEncoding.EBCDIC);
    public static readonly TagFinderByte EBCDIC_Turkish = new(ByteEncoding.EBCDIC_Turkish);
    public static readonly TagFinderByte IBM_Latin1 = new(ByteEncoding.IBM_Latin1);

    public TagFinderByte(Tokens tokens, bool[]? otherSpaces) : base(tokens)
    {
        _otherSpaces = otherSpaces;
    }

    public TagFinderByte(ByteEncoding byteEncoding) 
        : base(MapTokens(byteEncoding._tokens))
    {
        if (byteEncoding == null) throw new ArgumentNullException(nameof(byteEncoding));

        _otherSpaces = byteEncoding._otherSpaces;
    }

    private static Tokens MapTokens(ByteEncoding.Tokens tokens)
    {
        return new(tokens._lt, tokens._gt, tokens._slash, tokens._colon, 
            tokens._space, tokens._quot, tokens._apos, tokens._eq);
    }

    protected override bool IsSpace(byte value)
        => value.Equals(_tokens._space) || _otherSpaces != null && _otherSpaces[value];

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out TagFinderByte byteTagFinder)
    {
        if (Array.IndexOf(ByteEncoding.Utf8_CodePages, codePage) > -1)
        {
            byteTagFinder = Utf8;
            return true;
        }
        if (codePage == 29001)
        {
            byteTagFinder = Europa;
            return true;
        }
        if (Array.IndexOf(ByteEncoding.EBCDIC_CodePages, codePage) > -1)
        {
            byteTagFinder = EBCDIC;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            byteTagFinder = EBCDIC_Turkish;
            return true;
        }
        if (codePage == 1047 || codePage == 20924)
        {
            byteTagFinder = IBM_Latin1;
            return true;
        }
        byteTagFinder = null;
        return false;
    }
}