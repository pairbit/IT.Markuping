using IT.Markuping.Implementation;
using IT.Markuping.Interfaces;
using IT.Markuping.Internal;
using System;

namespace IT.Markuping;

public static class MarkupFinders
{
    internal static class OtherSpaces
    {
        public static readonly OtherSpacesTagFinder<byte> Utf8 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Utf8);
        public static readonly OtherSpaceTagFinder<byte> Europa = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Europa);
        public static readonly OtherSpacesTagFinder<byte> EBCDIC = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC);
        public static readonly OtherSpacesTagFinder<byte> EBCDIC_Turkish = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC_Turkish);
        public static readonly OtherSpacesTagFinder<byte> IBM_Latin1 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.IBM_Latin1);
    }

    internal static class Complex
    {
        internal static readonly ComplexTagFinder<byte> Utf16 = new(MarkupAlphabets.Byte.Utf16);
        internal static readonly ComplexTagFinder<byte> Utf16BE = new(MarkupAlphabets.Byte.Utf16BE);
        internal static readonly ComplexTagFinder<byte> Utf32 = new(MarkupAlphabets.Byte.Utf32);
        internal static readonly ComplexTagFinder<byte> Utf32BE = new(MarkupAlphabets.Byte.Utf32BE);
    }

    public static class Char
    {
        public static readonly TagFinderChar Utf16 = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16);
        public static readonly TagFinderChar Utf16BE = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16BE);
#if NET
        public static readonly TagFinderChar Utf16_IgnoreCase = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16, StringComparison.OrdinalIgnoreCase);
#endif
    }

    public static class Int32
    {
        public static readonly OtherSpacesTagFinder<int> Utf32 = new((MarkupTokens<int>)MarkupAlphabets.Int32.Utf32);
        public static readonly OtherSpacesTagFinder<int> Utf32BE = new((MarkupTokens<int>)MarkupAlphabets.Int32.Utf32BE);
    }

    public static readonly TagFinderByte Utf8 = TagFinderByte.Utf8;
    public static readonly TagFinderByte Europa = TagFinderByte.Europa;
    public static readonly TagFinderByte EBCDIC = TagFinderByte.EBCDIC;
    public static readonly TagFinderByte EBCDIC_Turkish = TagFinderByte.EBCDIC_Turkish;
    public static readonly TagFinderByte IBM_Latin1 = TagFinderByte.IBM_Latin1;

    public static readonly ProxyMarkupFinderByte<char> Utf16 = new(Char.Utf16);
    public static readonly ProxyMarkupFinderByte<char> Utf16BE = new(Char.Utf16BE);
    public static readonly ProxyMarkupFinderByte<int> Utf32 = new(Int32.Utf32);
    public static readonly ProxyMarkupFinderByte<int> Utf32BE = new(Int32.Utf32BE);

    public static bool TryGet(int codePage,
#if !NETSTANDARD2_0
        [System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)]
#endif
        out IMarkupFinder<byte> tagFinder
    )
    {
        if (MarkupCodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
        {
            tagFinder = Utf8;
            return true;
        }
        if (codePage == 29001)
        {
            tagFinder = Europa;
            return true;
        }
        if (MarkupCodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
        {
            tagFinder = EBCDIC;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            tagFinder = EBCDIC_Turkish;
            return true;
        }
        if (codePage == 1047 || codePage == 20924)
        {
            tagFinder = IBM_Latin1;
            return true;
        }
        if (codePage == 1200)
        {
            tagFinder = Utf16;
            return true;
        }
        if (codePage == 1201)
        {
            tagFinder = Utf16BE;
            return true;
        }
        if (codePage == 12000)
        {
            tagFinder = Utf32;
            return true;
        }
        if (codePage == 12001)
        {
            tagFinder = Utf32BE;
            return true;
        }
        tagFinder = null
#if NETSTANDARD2_0
        !
#endif
            ;
        return false;
    }

    /*
     public static bool TryGetStrict(int codePage, [MaybeNullWhen(false)] out ITagFinder<byte> tagFinder)
    {
        if (Utf8_Strict.CodePages.IndexOf(codePage) > -1)
        {
            tagFinder = Utf8_Strict;
            return true;
        }
        if (EBCDIC_Strict.CodePages.IndexOf(codePage) > -1)
        {
            tagFinder = EBCDIC_Strict;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            tagFinder = EBCDIC_Turkish_Strict;
            return true;
        }
        if (codePage == 1200)
        {
            tagFinder = Utf16_Strict;
            return true;
        }
        if (codePage == 1201)
        {
            tagFinder = Utf16BE_Strict;
            return true;
        }
        if (codePage == 12000)
        {
            tagFinder = Utf32_Strict;
            return true;
        }
        if (codePage == 12001)
        {
            tagFinder = Utf32BE_Strict;
            return true;
        }
        tagFinder = null;
        return false;
    }
     */
}