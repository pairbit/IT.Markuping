namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Int32
    {
        public static readonly MarkupAlphabet<int> Utf32_Strict =
            new(1, ['<', '>', '/', ':', ' ', '"', '=']);

        public static readonly MarkupAlphabet<int> Utf32BE_Strict =
            new(1, [1006632960, 1040187392, 788529152, 973078528, 536870912, 570425344, 1023410176]);

        public static readonly MarkupAlphabet<int> Utf32 =
            new(1, ['<', '>', '/', ':', ' ', '"', '=',
                '\'', '\r', '\n', '\t']);

        public static readonly MarkupAlphabet<int> Utf32BE =
            new(1, [1006632960, 1040187392, 788529152, 973078528, 536870912, 570425344, 1023410176,
                654311424, 218103808, 167772160, 150994944]);
    }
}