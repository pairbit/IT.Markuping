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
    }
}