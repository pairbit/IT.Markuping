namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Char
    {
        public static readonly MarkupAlphabet<char> Utf16 =
            new(1, ['<', '>', '/', ':', ' ', '"', '=',
                '!', '-', '[', ']', '?',
                'x', 'm', 'l', 'n', 's', 'i', 'd',
                '\'', '\r', '\n', '\t']);

        public static readonly MarkupAlphabet<char> Utf16BE =
            new(1, [(char)15360, (char)15872, (char)12032, (char)14848, (char)8192, (char)8704, (char)15616,
                (char)8448, (char)11520, (char)23296, (char)23808, (char)16128,
                (char)30720, (char)27904, (char)27648, (char)28160, (char)29440, (char)26880, (char)25600,
                (char)9984, (char)3328, (char)2560, (char)2304]);
    }
}