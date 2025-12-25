using System;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Byte
    {
        public static readonly MarkupAlphabet<byte> Utf8_Strict =
            new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=']);

        public static readonly MarkupAlphabet<byte> EBCDIC_Strict =
            new(1, [76, 110, 97, 122, 64, 127, 126]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Turkish_Strict =
            new(1, [76, 110, 97, 122, 64, 252, 126]);

        public static readonly MarkupAlphabet<byte> Utf8 =
            new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'\'', (byte)'\r', (byte)'\n', (byte)'\t']);

        public static readonly MarkupAlphabet<byte> Europa =
            new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'=',
                (byte)'\'', (byte)'?', (byte)'?', (byte)'?']);

        public static readonly MarkupAlphabet<byte> EBCDIC =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> EBCDIC_Turkish =
            new(1, [76, 110, 97, 122, 64, 252, 126,
                125, 13, 37, 5]);

        public static readonly MarkupAlphabet<byte> IBM_Latin1 =
            new(1, [76, 110, 97, 122, 64, 127, 126,
                125, 13, 21, 5]);

        public static readonly MarkupAlphabet<byte> Utf16_Strict =
            new(2, [(byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=', 0]);

        public static readonly MarkupAlphabet<byte> Utf16BE_Strict =
            new(2, [0, (byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=']);

        public static readonly MarkupAlphabet<byte> Utf32_Strict =
            new(4, [(byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=', 0, 0, 0]);

        public static readonly MarkupAlphabet<byte> Utf32BE_Strict =
            new(4, [0, 0, 0, (byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=']);

        public static readonly MarkupAlphabet<byte> Utf16 =
            new(2, [(byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=', 0,
                (byte)'\'', 0, (byte)'\r', 0, (byte)'\n', 0, (byte)'\t', 0]);

        public static readonly MarkupAlphabet<byte> Utf16BE =
            new(2, [0, (byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'=',
                0, (byte)'\'', 0, (byte)'\r', 0, (byte)'\n', 0, (byte)'\t']);

        public static readonly MarkupAlphabet<byte> Utf32 =
            new(4, [(byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=', 0, 0, 0,
                (byte)'\'', 0, 0, 0, (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t', 0, 0, 0]);

        public static readonly MarkupAlphabet<byte> Utf32BE =
            new(4, [0, 0, 0, (byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'=',
                0, 0, 0, (byte)'\'', 0, 0, 0, (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t']);

        public static bool TryGet(int codePage, [MaybeNullWhen(false)] out MarkupAlphabet<byte> alphabet)
        {
            if (MarkupCodePages.Utf8.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = Utf8;
                return true;
            }
            if (codePage == 29001)
            {
                alphabet = Europa;
                return true;
            }
            if (MarkupCodePages.EBCDIC.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = EBCDIC;
                return true;
            }
            if (codePage == 1026 || codePage == 20905)
            {
                alphabet = EBCDIC_Turkish;
                return true;
            }
            if (codePage == 1047 || codePage == 20924)
            {
                alphabet = IBM_Latin1;
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

        public static bool TryGetStrict(int codePage, [MaybeNullWhen(false)] out MarkupAlphabet<byte> alphabet)
        {
            if (MarkupCodePages.Utf8_Strict.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = Utf8_Strict;
                return true;
            }
            if (MarkupCodePages.EBCDIC_Strict.AsSpan().IndexOf(codePage) > -1)
            {
                alphabet = EBCDIC_Strict;
                return true;
            }
            if (codePage == 1026 || codePage == 20905)
            {
                alphabet = EBCDIC_Turkish_Strict;
                return true;
            }
            if (codePage == 1200)
            {
                alphabet = Utf16_Strict;
                return true;
            }
            if (codePage == 1201)
            {
                alphabet = Utf16BE_Strict;
                return true;
            }
            if (codePage == 12000)
            {
                alphabet = Utf32_Strict;
                return true;
            }
            if (codePage == 12001)
            {
                alphabet = Utf32BE_Strict;
                return true;
            }
            alphabet = default;
            return false;
        }
    }
}