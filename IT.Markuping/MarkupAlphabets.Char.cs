namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Char
    {
        public static readonly MarkupAlphabet<char> Utf16_Strict =
            new(1, ['<', '>', '/', ':', ' ', '"', '=']);

        public static readonly MarkupAlphabet<char> Utf16BE_Strict =
            new(1, [(char)15360, (char)15872, (char)12032, (char)14848, (char)8192, (char)8704, (char)15616]);

        public static readonly MarkupAlphabet<char> Utf16 =
            new(1, ['<', '>', '/', ':', ' ', '"', '=',
                '\'', '\r', '\n', '\t']);

        public static readonly MarkupAlphabet<char> Utf16BE =
            new(1, [(char)15360, (char)15872, (char)12032, (char)14848, (char)8192, (char)8704, (char)15616,
                (char)9984, (char)3328, (char)2560, (char)2304]);
    }
}