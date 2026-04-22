using IT.Markuping.Internal;
using System;
using System.Diagnostics;

namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Byte
    {
        //<>/: "=!-[]?xmlnsidID'\r\n\t
        public static readonly MarkupAlphabet<byte> Utf8 =
            new([(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'!', (byte)'-', (byte)'[', (byte)']', (byte)'?',
                (byte)'x', (byte)'m', (byte)'l', (byte)'n', (byte)'s', (byte)'i', (byte)'d', (byte)'I', (byte)'D',
                (byte)'\'', (byte)'\r', (byte)'\n', (byte)'\t']);

        //unknown '?'
        public static readonly MarkupAlphabet<byte> IA5 =
            new([(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'!', (byte)'-', (byte)'?', (byte)'?', (byte)'?',
                (byte)'x', (byte)'m', (byte)'l', (byte)'n', (byte)'s', (byte)'i', (byte)'d', (byte)'I', (byte)'D',
                (byte)'\'', (byte)'\r', (byte)'\n', (byte)'\t']);

        //unknown '?'
        public static readonly MarkupAlphabet<byte> Europa =
            new([(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'!', (byte)'-', (byte)'[', (byte)']', (byte)'?',
                (byte)'x', (byte)'m', (byte)'l', (byte)'n', (byte)'s', (byte)'i', (byte)'d', (byte)'I', (byte)'D',
                (byte)'\'', (byte)'?', (byte)'?', (byte)'?']);

        public static readonly MarkupAlphabet<byte> EBCDIC =
            new([76, 110, 97, 122, 64, 127, 126,
                79, 96, 74, 90, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Canada_Hebrew =
            new([76, 110, 97, 122, 64, 127, 126,
                90, 96, 186, 187, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_IBM_Latin1 =
            new([76, 110, 97, 122, 64, 127, 126,
                90, 96, 173, 189, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 21, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Germany =
            new([76, 110, 97, 122, 64, 127, 126,
                79, 96, 99, 252, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Denmark_Norway =
            new([76, 110, 97, 122, 64, 127, 126,
                79, 96, 158, 159, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Finland_Sweden =
            new([76, 110, 97, 122, 64, 127, 126,
                79, 96, 181, 159, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Italy =
            new([76, 110, 97, 122, 64, 127, 126,
                79, 96, 144, 81, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Spain =
            new([76, 110, 97, 122, 64, 127, 126,
                187, 96, 74, 90, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_UK =
            new([76, 110, 97, 122, 64, 127, 126,
                90, 96, 177, 187, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_France =
            new([76, 110, 97, 122, 64, 127, 126,
                79, 96, 144, 181, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Icelandic =
            new([76, 110, 97, 122, 64, 127, 126,
                79, 96, 174, 158, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Turkish_Latin5 =
            new([76, 110, 97, 122, 64, 252, 126,
                79, 96, 104, 172, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Japanese_katakana =
            new([76, 110, 97, 122, 64, 127, 126,
                90, 96, 112, 128, 111, 183, 117, 116, 118, 171, 113, 101, 201, 196,
                125, 13, 37, 5]);

        //unknown '?' = 111
        public static readonly MarkupAlphabet<byte> EBCDIC_Arabic =
            new([76, 110, 97, 122, 64, 127, 126,
                90, 96, 111, 111, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Korean_Extended =
            new([76, 110, 97, 122, 64, 127, 126,
                90, 96, 112, 128, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Thai =
            new([76, 110, 97, 122, 64, 127, 126,
                90, 96, 73, 89, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Turkish =
            new([76, 110, 97, 122, 64, 252, 126,
                79, 96, 104, 182, 111, 167, 148, 147, 149, 162, 137, 132, 201, 196,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> Utf16 =
            new([(byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=', 0,
                (byte)'!', 0, (byte)'-', 0, (byte)'[', 0, (byte)']', 0, (byte)'?', 0,
                (byte)'x', 0, (byte)'m', 0, (byte)'l', 0, (byte)'n', 0, (byte)'s', 0, (byte)'i', 0, (byte)'d', 0, (byte)'I', 0, (byte)'D', 0,
                (byte)'\'', 0, (byte)'\r', 0, (byte)'\n', 0, (byte)'\t', 0]);

        public static readonly MarkupAlphabet<byte> Utf16BE =
            new([0, (byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=',
                0, (byte)'!', 0, (byte)'-', 0, (byte)'[', 0, (byte)']', 0, (byte)'?',
                0, (byte)'x', 0, (byte)'m', 0, (byte)'l', 0, (byte)'n', 0, (byte)'s', 0, (byte)'i', 0, (byte)'d', 0, (byte)'I', 0, (byte)'D',
                0, (byte)'\'', 0, (byte)'\r', 0, (byte)'\n', 0, (byte)'\t']);

        public static readonly MarkupAlphabet<byte> Utf32 =
            new([(byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=', 0, 0, 0,
                (byte)'!', 0, 0, 0, (byte)'-', 0, 0, 0, (byte)'[', 0, 0, 0, (byte)']', 0, 0, 0, (byte)'?', 0, 0, 0,
                (byte)'x', 0, 0, 0, (byte)'m', 0, 0, 0, (byte)'l', 0, 0, 0, (byte)'n', 0, 0, 0, (byte)'s', 0, 0, 0, (byte)'i', 0, 0, 0, (byte)'d', 0, 0, 0, (byte)'I', 0, 0, 0, (byte)'D', 0, 0, 0,
                (byte)'\'', 0, 0, 0, (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t', 0, 0, 0]);

        public static readonly MarkupAlphabet<byte> Utf32BE =
            new([0, 0, 0, (byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=',
                0, 0, 0, (byte)'!', 0, 0, 0, (byte)'-', 0, 0, 0, (byte)'[', 0, 0, 0, (byte)']', 0, 0, 0, (byte)'?',
                0, 0, 0, (byte)'x', 0, 0, 0, (byte)'m', 0, 0, 0, (byte)'l', 0, 0, 0, (byte)'n', 0, 0, 0, (byte)'s', 0, 0, 0, (byte)'i', 0, 0, 0, (byte)'d', 0, 0, 0, (byte)'I', 0, 0, 0, (byte)'D',
                0, 0, 0, (byte)'\'', 0, 0, 0, (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t']);

        public static bool TryGet(int codePage, out MarkupAlphabet<byte> alphabet)
        {
            if (CodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = Utf8;
                return true;
            }
            if (codePage == CodePage.Utf16)
            {
                alphabet = Utf16;
                return true;
            }
            if (codePage == CodePage.Utf16BE)
            {
                alphabet = Utf16BE;
                return true;
            }
            if (codePage == CodePage.Utf32)
            {
                alphabet = Utf32;
                return true;
            }
            if (codePage == CodePage.Utf32BE)
            {
                alphabet = Utf32BE;
                return true;
            }
            if (codePage == 20106 || codePage == 20107 || codePage == 20108)
            {
                Debug.Assert(CodePages.IA5.AsSpan().IndexOf(codePage) > -1);
                alphabet = IA5;
                return true;
            }
            if (codePage == CodePage.Europa)
            {
                alphabet = Europa;
                return true;
            }
            if (CodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = EBCDIC;
                return true;
            }
            if (codePage == 37 || codePage == 1140 || codePage == 20424)
            {
                Debug.Assert(CodePages.EBCDIC_Canada_Hebrew.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_Canada_Hebrew;
                return true;
            }
            if (codePage == 1047 || codePage == 20924)
            {
                Debug.Assert(CodePages.EBCDIC_IBM_Latin1.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_IBM_Latin1;
                return true;
            }
            if (codePage == 1141 || codePage == 20273)
            {
                Debug.Assert(CodePages.EBCDIC_Germany.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_Germany;
                return true;
            }
            if (codePage == 1142 || codePage == 20277)
            {
                Debug.Assert(CodePages.EBCDIC_Denmark_Norway.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_Denmark_Norway;
                return true;
            }
            if (codePage == 1143 || codePage == 20278)
            {
                Debug.Assert(CodePages.EBCDIC_Finland_Sweden.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_Finland_Sweden;
                return true;
            }
            if (codePage == 1144 || codePage == 20280)
            {
                Debug.Assert(CodePages.EBCDIC_Italy.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_Italy;
                return true;
            }
            if (codePage == 1145 || codePage == 20284)
            {
                Debug.Assert(CodePages.EBCDIC_Spain.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_Spain;
                return true;
            }
            if (codePage == 1146 || codePage == 20285)
            {
                Debug.Assert(CodePages.EBCDIC_UK.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_UK;
                return true;
            }
            if (codePage == 1147 || codePage == 20297)
            {
                Debug.Assert(CodePages.EBCDIC_France.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_France;
                return true;
            }
            if (codePage == 1149 || codePage == 20871)
            {
                Debug.Assert(CodePages.EBCDIC_Icelandic.AsSpan().IndexOf(codePage) > -1);
                alphabet = EBCDIC_Icelandic;
                return true;
            }
            if (codePage == CodePage.EBCDIC_Turkish_Latin5)
            {
                alphabet = EBCDIC_Turkish_Latin5;
                return true;
            }
            if (codePage == CodePage.EBCDIC_Japanese_katakana)
            {
                alphabet = EBCDIC_Japanese_katakana;
                return true;
            }
            if (codePage == CodePage.EBCDIC_Arabic)
            {
                alphabet = EBCDIC_Arabic;
                return true;
            }
            if (codePage == CodePage.EBCDIC_Korean_Extended)
            {
                alphabet = EBCDIC_Korean_Extended;
                return true;
            }
            if (codePage == CodePage.EBCDIC_Thai)
            {
                alphabet = EBCDIC_Thai;
                return true;
            }
            if (codePage == CodePage.EBCDIC_Turkish)
            {
                alphabet = EBCDIC_Turkish;
                return true;
            }
            alphabet = default;
            return false;
        }

        public static bool TryGetInfo(int codePage, out MarkupAlphabetInfo<byte> alphabetInfo)
        {
            if (CodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
            {
                alphabetInfo = new(Utf8, CodePages.Utf8);
                return true;
            }
            if (codePage == CodePage.Utf16)
            {
                alphabetInfo = new(Utf16, CodePages.Utf16);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.Utf16BE)
            {
                alphabetInfo = new(Utf16BE, CodePages.Utf16BE);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.Utf32)
            {
                alphabetInfo = new(Utf32, CodePages.Utf32);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.Utf32BE)
            {
                alphabetInfo = new(Utf32BE, CodePages.Utf32BE);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 20106 || codePage == 20107 || codePage == 20108)
            {
                alphabetInfo = new(IA5, CodePages.IA5);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.Europa)
            {
                alphabetInfo = new(Europa, CodePages.Europa);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (CodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
            {
                alphabetInfo = new(EBCDIC, CodePages.EBCDIC);
                return true;
            }
            if (codePage == 37 || codePage == 1140 || codePage == 20424)
            {
                alphabetInfo = new(EBCDIC_Canada_Hebrew, CodePages.EBCDIC_Canada_Hebrew);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1047 || codePage == 20924)
            {
                alphabetInfo = new(EBCDIC_IBM_Latin1, CodePages.EBCDIC_IBM_Latin1);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1141 || codePage == 20273)
            {
                Debug.Assert(CodePages.EBCDIC_Germany.AsSpan().IndexOf(codePage) > -1);
                alphabetInfo = new(EBCDIC_Germany, CodePages.EBCDIC_Germany);
                return true;
            }
            if (codePage == 1142 || codePage == 20277)
            {
                alphabetInfo = new(EBCDIC_Denmark_Norway, CodePages.EBCDIC_Denmark_Norway);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1143 || codePage == 20278)
            {
                alphabetInfo = new(EBCDIC_Finland_Sweden, CodePages.EBCDIC_Finland_Sweden);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1144 || codePage == 20280)
            {
                alphabetInfo = new(EBCDIC_Italy, CodePages.EBCDIC_Italy);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1145 || codePage == 20284)
            {
                alphabetInfo = new(EBCDIC_Spain, CodePages.EBCDIC_Spain);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1146 || codePage == 20285)
            {
                alphabetInfo = new(EBCDIC_UK, CodePages.EBCDIC_UK);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1147 || codePage == 20297)
            {
                alphabetInfo = new(EBCDIC_France, CodePages.EBCDIC_France);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == 1149 || codePage == 20871)
            {
                alphabetInfo = new(EBCDIC_Icelandic, CodePages.EBCDIC_Icelandic);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.EBCDIC_Turkish_Latin5)
            {
                alphabetInfo = new(EBCDIC_Turkish_Latin5, CodePages.EBCDIC_Turkish_Latin5);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.EBCDIC_Japanese_katakana)
            {
                alphabetInfo = new(EBCDIC_Japanese_katakana, CodePages.EBCDIC_Japanese_katakana);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.EBCDIC_Arabic)
            {
                alphabetInfo = new(EBCDIC_Arabic, CodePages.EBCDIC_Arabic);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.EBCDIC_Korean_Extended)
            {
                alphabetInfo = new(EBCDIC_Korean_Extended, CodePages.EBCDIC_Korean_Extended);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.EBCDIC_Thai)
            {
                alphabetInfo = new(EBCDIC_Thai, CodePages.EBCDIC_Thai);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            if (codePage == CodePage.EBCDIC_Turkish)
            {
                alphabetInfo = new(EBCDIC_Turkish, CodePages.EBCDIC_Turkish);
                Debug.Assert(alphabetInfo.CodePages.IndexOf(codePage) > -1);
                return true;
            }
            alphabetInfo = default;
            return false;
        }
    }
}