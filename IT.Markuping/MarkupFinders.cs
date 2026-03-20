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

        public static readonly int[] Utf16 = [1200];
        public static readonly int[] Utf16BE = [1201];
        public static readonly int[] Utf32 = [12000];
        public static readonly int[] Utf32BE = [12001];

        public static readonly int[] Europa = [29001];

        public static readonly int[] EBCDIC =
            [37, 500, 870, 875, 1140, 1141, 1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 20273, 20277, 20278, 20280, 20284, 20285, 20290, 20297, 20420, 20423, 20424, 20833, 20838, 20871, 20880, 21025];

        public static readonly int[] EBCDIC_Turkish = [1026, 20905];

        public static readonly int[] EBCDIC_IBM_Latin1 = [1047, 20924];
    }

    internal static class OtherSpaces
    {
        public static readonly OtherSpacesMarkupFinder<byte> Utf8 = new(CodePages.Utf8, (MarkupTokens<byte>)MarkupAlphabets.Byte.Utf8);
        public static readonly OtherSpaceMarkupFinder<byte> Europa = new(CodePages.Europa, (MarkupTokens<byte>)MarkupAlphabets.Byte.Europa);
        public static readonly OtherSpacesMarkupFinder<byte> EBCDIC = new(CodePages.EBCDIC, (MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC);
        public static readonly OtherSpacesMarkupFinder<byte> EBCDIC_Turkish = new(CodePages.EBCDIC_Turkish, (MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC_Turkish);
        public static readonly OtherSpacesMarkupFinder<byte> EBCDIC_IBM_Latin1 = new(CodePages.EBCDIC_IBM_Latin1, (MarkupTokens<byte>)MarkupAlphabets.Byte.EBCDIC_IBM_Latin1);
    }

    internal static class Complex
    {
        internal static readonly ComplexMarkupFinder<byte> Utf16 = new(CodePages.Utf16, MarkupAlphabets.Byte.Utf16);
        internal static readonly ComplexMarkupFinder<byte> Utf16BE = new(CodePages.Utf16BE, MarkupAlphabets.Byte.Utf16BE);
        internal static readonly ComplexMarkupFinder<byte> Utf32 = new(CodePages.Utf32, MarkupAlphabets.Byte.Utf32);
        internal static readonly ComplexMarkupFinder<byte> Utf32BE = new(CodePages.Utf32BE, MarkupAlphabets.Byte.Utf32BE);
    }

    public static class Char
    {
#if NET
        public static readonly MarkupFinderChar Utf16_IgnoreCase = new(CodePages.Utf16, (MarkupTokens<char>)MarkupAlphabets.Char.Utf16, StringComparison.OrdinalIgnoreCase);
#endif
        public static readonly MarkupFinderChar Utf16 = new(CodePages.Utf16, (MarkupTokens<char>)MarkupAlphabets.Char.Utf16);
        public static readonly MarkupFinderChar Utf16BE = new(CodePages.Utf16BE, (MarkupTokens<char>)MarkupAlphabets.Char.Utf16BE);
    }

    public static class Int32
    {
        public static readonly OtherSpacesMarkupFinder<int> Utf32 = new(CodePages.Utf32, (MarkupTokens<int>)MarkupAlphabets.Int32.Utf32);
        public static readonly OtherSpacesMarkupFinder<int> Utf32BE = new(CodePages.Utf32BE, (MarkupTokens<int>)MarkupAlphabets.Int32.Utf32BE);
    }

    public static readonly MarkupFinderByte Utf8 = MarkupFinderByte.Utf8;
    public static readonly MarkupFinderByte Europa = MarkupFinderByte.Europa;
    public static readonly MarkupFinderByte EBCDIC = MarkupFinderByte.EBCDIC;
    public static readonly MarkupFinderByte EBCDIC_Turkish = MarkupFinderByte.EBCDIC_Turkish;
    public static readonly MarkupFinderByte EBCDIC_IBM_Latin1 = MarkupFinderByte.EBCDIC_IBM_Latin1;

#if NET
    public static readonly ProxyMarkupFinderByte<char> Utf16_IgnoreCase = new(Char.Utf16_IgnoreCase);
#endif

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
        if (Utf8.CodePages.IndexOf(codePage) > -1)
        {
            finder = Utf8;
            return true;
        }
        if (codePage == 1200)
        {
            finder = Utf16;
            Debug.Assert(finder.CodePages.IndexOf(codePage) > -1);
            return true;
        }
        if (codePage == 1201)
        {
            finder = Utf16BE;
            Debug.Assert(finder.CodePages.IndexOf(codePage) > -1);
            return true;
        }
        if (codePage == 12000)
        {
            finder = Utf32;
            Debug.Assert(finder.CodePages.IndexOf(codePage) > -1);
            return true;
        }
        if (codePage == 12001)
        {
            finder = Utf32BE;
            Debug.Assert(finder.CodePages.IndexOf(codePage) > -1);
            return true;
        }
        if (codePage == 29001)
        {
            finder = Europa;
            Debug.Assert(finder.CodePages.IndexOf(codePage) > -1);
            return true;
        }
        if (EBCDIC.CodePages.IndexOf(codePage) > -1)
        {
            finder = EBCDIC;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            finder = EBCDIC_Turkish;
            Debug.Assert(finder.CodePages.IndexOf(codePage) > -1);
            return true;
        }
        if (codePage == 1047 || codePage == 20924)
        {
            finder = EBCDIC_IBM_Latin1;
            Debug.Assert(finder.CodePages.IndexOf(codePage) > -1);
            return true;
        }
        finder = null
#if NETSTANDARD2_0
        !
#endif
            ;
        return false;
    }

    /// <exception cref="NotSupportedException">MarkupFinder does not support code page</exception>
    public static IMarkupFinder<byte> Get(int codePage)
    {
        if (TryGet(codePage, out var decl)) return decl;

        throw new NotSupportedException($"MarkupFinder does not support code page {codePage}.");
    }
}