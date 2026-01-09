using IT.Markuping.Implementation;
using IT.Markuping.Interfaces;
using System;
using System.Diagnostics;

namespace IT.Markuping;

public static class MarkupFinders
{
    internal static class CodePages
    {
        public static readonly int[] Utf8 =
            [437, 708, 720, 737, 775, 850, 852, 855, 857, 858, 860, 861, 862, 863, 864, 865, 866, 869, 874, 932, 936, 949, 950, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1361, 10000, 10001, 10002, 10003, 10004, 10005, 10006, 10007, 10008, 10010, 10017, 10021, 10029, 10079, 10081, 10082, 20000, 20001, 20002, 20003, 20004, 20005, 20105, 20106, 20107, 20108, 20127, 20261, 20269, 20866, 20932, 20936, 20949, 21866, 28591, 28592, 28593, 28594, 28595, 28596, 28597, 28598, 28599, 28603, 28605, 38598, 50220, 50221, 50222, 50225, 50227, 51932, 51936, 51949, 52936, 54936, 57002, 57003, 57004, 57005, 57006, 57007, 57008, 57009, 57010, 57011, 65001];

        public static readonly int[] EBCDIC =
            [37, 500, 870, 875, 1140, 1141, 1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 20273, 20277, 20278, 20280, 20284, 20285, 20290, 20297, 20420, 20423, 20424, 20833, 20838, 20871, 20880, 21025];

        public static readonly int[] EBCDIC_Turkish = [1026, 20905];

        public static readonly int[] EBCDIC_IBM_Latin1 = [1047, 20924];
    }

    internal static class OtherSpaces
    {
        public static readonly OtherSpacesMarkupFinder<byte> Utf8 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Utf8);
        public static readonly OtherSpaceMarkupFinder<byte> Europa = new((MarkupTokens<byte>)MarkupAlphabets.Byte.Europa);
        public static readonly OtherSpacesMarkupFinder<byte> EBCDIC = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC);
        public static readonly OtherSpacesMarkupFinder<byte> EBCDIC_Turkish = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC_Turkish);
        public static readonly OtherSpacesMarkupFinder<byte> EBCDIC_IBM_Latin1 = new((MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC_IBM_Latin1);
    }

    internal static class Complex
    {
        internal static readonly ComplexMarkupFinder<byte> Utf16 = new(MarkupAlphabets.Byte.Utf16);
        internal static readonly ComplexMarkupFinder<byte> Utf16BE = new(MarkupAlphabets.Byte.Utf16BE);
        internal static readonly ComplexMarkupFinder<byte> Utf32 = new(MarkupAlphabets.Byte.Utf32);
        internal static readonly ComplexMarkupFinder<byte> Utf32BE = new(MarkupAlphabets.Byte.Utf32BE);
    }

    public static class Char
    {
        public static readonly MarkupFinderChar Utf16 = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16);
        public static readonly MarkupFinderChar Utf16BE = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16BE);
#if NET
        public static readonly MarkupFinderChar Utf16_IgnoreCase = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16, StringComparison.OrdinalIgnoreCase);
#endif
    }

    public static class Int32
    {
        public static readonly OtherSpacesMarkupFinder<int> Utf32 = new((MarkupTokens<int>)MarkupAlphabets.Int32.Utf32);
        public static readonly OtherSpacesMarkupFinder<int> Utf32BE = new((MarkupTokens<int>)MarkupAlphabets.Int32.Utf32BE);
    }

    public static readonly MarkupFinderByte Utf8 = MarkupFinderByte.Utf8;
    public static readonly MarkupFinderByte Europa = MarkupFinderByte.Europa;
    public static readonly MarkupFinderByte EBCDIC = MarkupFinderByte.EBCDIC;
    public static readonly MarkupFinderByte EBCDIC_Turkish = MarkupFinderByte.EBCDIC_Turkish;
    public static readonly MarkupFinderByte IBM_Latin1 = MarkupFinderByte.IBM_Latin1;

    public static readonly ProxyMarkupFinderByte<char> Utf16 = new(Char.Utf16);
    public static readonly ProxyMarkupFinderByte<char> Utf16BE = new(Char.Utf16BE);
    public static readonly ProxyMarkupFinderByte<int> Utf32 = new(Int32.Utf32);
    public static readonly ProxyMarkupFinderByte<int> Utf32BE = new(Int32.Utf32BE);

    public static bool TryGet(int codePage,
#if !NETSTANDARD2_0
        [System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)]
#endif
        out IMarkupFinder<byte> finder
    )
    {
        if (CodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
        {
            finder = Utf8;
            return true;
        }
        if (codePage == 29001)
        {
            finder = Europa;
            return true;
        }
        if (CodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
        {
            finder = EBCDIC;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            Debug.Assert(CodePages.EBCDIC_Turkish.IndexOf(codePage) > -1);
            finder = EBCDIC_Turkish;
            return true;
        }
        if (codePage == 1047 || codePage == 20924)
        {
            Debug.Assert(CodePages.EBCDIC_IBM_Latin1.IndexOf(codePage) > -1);
            finder = IBM_Latin1;
            return true;
        }
        if (codePage == 1200)
        {
            finder = Utf16;
            return true;
        }
        if (codePage == 1201)
        {
            finder = Utf16BE;
            return true;
        }
        if (codePage == 12000)
        {
            finder = Utf32;
            return true;
        }
        if (codePage == 12001)
        {
            finder = Utf32BE;
            return true;
        }
        finder = null
#if NETSTANDARD2_0
        !
#endif
            ;
        return false;
    }

    /*
     public static bool TryGetStrict(int codePage, [MaybeNullWhen(false)] out IMarkupFinder<byte> finder)
    {
        if (Utf8_Strict.CodePages.IndexOf(codePage) > -1)
        {
            finder = Utf8_Strict;
            return true;
        }
        if (EBCDIC_Strict.CodePages.IndexOf(codePage) > -1)
        {
            finder = EBCDIC_Strict;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            finder = EBCDIC_Turkish_Strict;
            return true;
        }
        if (codePage == 1200)
        {
            finder = Utf16_Strict;
            return true;
        }
        if (codePage == 1201)
        {
            finder = Utf16BE_Strict;
            return true;
        }
        if (codePage == 12000)
        {
            finder = Utf32_Strict;
            return true;
        }
        if (codePage == 12001)
        {
            finder = Utf32BE_Strict;
            return true;
        }
        finder = null;
        return false;
    }
     */
}