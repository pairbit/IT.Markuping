namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Char
    {
        public static readonly MarkupAlphabet<char> Utf16 =
            new(['<', '>', '/', ':', ' ', '"', '=',
                '!', '-', '[', ']', '?',
                'x', 'm', 'l', 'n', 's', 'i', 'd', 'I', 'D',
                '\'', '\r', '\n', '\t']);
    }
}