namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class UInt32
    {
        public static readonly MarkupAlphabet<uint> Utf32 =
            new(['<', '>', '/', ':', ' ', '"', '=',
                '!', '-', '[', ']', '?',
                'x', 'm', 'l', 'n', 's', 'i', 'd', 'I', 'D',
                '\'', '\r', '\n', '\t']);
    }
}