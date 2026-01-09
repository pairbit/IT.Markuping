using System;
using System.Diagnostics;

namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Byte
    {
        //<>/: "=!-[]?xmlnsid'\r\n\t
        public static readonly MarkupAlphabet<byte> Utf8 =
            new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'!', (byte)'-', (byte)'[', (byte)']', (byte)'?',
                (byte)'x', (byte)'m', (byte)'l', (byte)'n', (byte)'s', (byte)'i', (byte)'d',
                (byte)'\'', (byte)'\r', (byte)'\n', (byte)'\t']);

        //unknown '?'
        public static readonly MarkupAlphabet<byte> IA5 =
            new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'!', (byte)'-', (byte)'?', (byte)'?', (byte)'?',
                (byte)'x', (byte)'m', (byte)'l', (byte)'n', (byte)'s', (byte)'i', (byte)'d',
                (byte)'\'', (byte)'\r', (byte)'\n', (byte)'\t']);

        //unknown '?'
        public static readonly MarkupAlphabet<byte> Europa =
            new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'!', (byte)'-', (byte)'[', (byte)']', (byte)'?',
                (byte)'x', (byte)'m', (byte)'l', (byte)'n', (byte)'s', (byte)'i', (byte)'d',
                (byte)'\'', (byte)'?', (byte)'?', (byte)'?']);

        public static readonly MarkupAlphabet<byte> EBCDIC =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                79, 96, 74, 90, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Canada_Hebrew =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                90, 96, 186, 187, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_IBM_Latin1 =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                90, 96, 173, 189, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 21, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Germany =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                79, 96, 99, 252, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Denmark_Norway =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                79, 96, 158, 159, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Finland_Sweden =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                79, 96, 181, 159, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Italy =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                79, 96, 144, 81, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Spain =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                187, 96, 74, 90, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_UK =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                90, 96, 177, 187, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_France =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                79, 96, 144, 181, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Icelandic =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                79, 96, 174, 158, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Turkish_Latin5 =
            new(1, [76, 110, 97, 122, 64, 252, 126,
                79, 96, 104, 172, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Japanese_katakana =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                90, 96, 112, 128, 111, 183, 117, 116, 118, 171, 113, 101,
                125, 13, 37, 5]);

        //unknown '?' = 111
        public static readonly MarkupAlphabet<byte> EBCDIC_Arabic =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                90, 96, 111, 111, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Korean_Extended =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                90, 96, 112, 128, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Thai =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                90, 96, 73, 89, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Turkish =
            new(1, [76, 110, 97, 122, 64, 252, 126,
                79, 96, 104, 182, 111, 167, 148, 147, 149, 162, 137, 132,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> Utf16 =
            new(2, [(byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=', 0,
                (byte)'!', 0, (byte)'-', 0, (byte)'[', 0, (byte)']', 0, (byte)'?', 0,
                (byte)'x', 0, (byte)'m', 0, (byte)'l', 0, (byte)'n', 0, (byte)'s', 0, (byte)'i', 0, (byte)'d', 0,
                (byte)'\'', 0, (byte)'\r', 0, (byte)'\n', 0, (byte)'\t', 0]);

        public static readonly MarkupAlphabet<byte> Utf16BE =
            new(2, [0, (byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=',
                0, (byte)'!', 0, (byte)'-', 0, (byte)'[', 0, (byte)']', 0, (byte)'?',
                0, (byte)'x', 0, (byte)'m', 0, (byte)'l', 0, (byte)'n', 0, (byte)'s', 0, (byte)'i', 0, (byte)'d',
                0, (byte)'\'', 0, (byte)'\r', 0, (byte)'\n', 0, (byte)'\t']);

        public static readonly MarkupAlphabet<byte> Utf32 =
            new(4, [(byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=', 0, 0, 0,
                (byte)'!', 0, 0, 0, (byte)'-', 0, 0, 0, (byte)'[', 0, 0, 0, (byte)']', 0, 0, 0, (byte)'?', 0, 0, 0,
                (byte)'x', 0, 0, 0, (byte)'m', 0, 0, 0, (byte)'l', 0, 0, 0, (byte)'n', 0, 0, 0, (byte)'s', 0, 0, 0, (byte)'i', 0, 0, 0, (byte)'d', 0, 0, 0,
                (byte)'\'', 0, 0, 0, (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t', 0, 0, 0]);

        public static readonly MarkupAlphabet<byte> Utf32BE =
            new(4, [0, 0, 0, (byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=',
                0, 0, 0, (byte)'!', 0, 0, 0, (byte)'-', 0, 0, 0, (byte)'[', 0, 0, 0, (byte)']', 0, 0, 0, (byte)'?',
                0, 0, 0, (byte)'x', 0, 0, 0, (byte)'m', 0, 0, 0, (byte)'l', 0, 0, 0, (byte)'n', 0, 0, 0, (byte)'s', 0, 0, 0, (byte)'i', 0, 0, 0, (byte)'d',
                0, 0, 0, (byte)'\'', 0, 0, 0, (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t']);

        public static bool TryGet(int codePage, out MarkupAlphabet<byte> alphabet)
        {
            if (CodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = Utf8;
                return true;
            }
            if (CodePages.IA5.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = IA5;
                return true;
            }
            if (codePage == CodePages.Europa)
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
            if (codePage == CodePages.EBCDIC_Turkish_Latin5)
            {
                alphabet = EBCDIC_Turkish_Latin5;
                return true;
            }
            if (codePage == CodePages.EBCDIC_Japanese_katakana)
            {
                alphabet = EBCDIC_Japanese_katakana;
                return true;
            }
            if (codePage == CodePages.EBCDIC_Arabic)
            {
                alphabet = EBCDIC_Arabic;
                return true;
            }
            if (codePage == CodePages.EBCDIC_Korean_Extended)
            {
                alphabet = EBCDIC_Korean_Extended;
                return true;
            }
            if (codePage == CodePages.EBCDIC_Thai)
            {
                alphabet = EBCDIC_Thai;
                return true;
            }
            if (codePage == CodePages.EBCDIC_Turkish)
            {
                alphabet = EBCDIC_Turkish;
                return true;
            }
            if (codePage == 1200)
            {
                alphabet = Utf16;
                return true;
            }
            if (codePage == 1201)
            {
                alphabet = Utf16BE;
                return true;
            }
            if (codePage == 12000)
            {
                alphabet = Utf32;
                return true;
            }
            if (codePage == 12001)
            {
                alphabet = Utf32BE;
                return true;
            }
            alphabet = default;
            return false;
        }
    }
}