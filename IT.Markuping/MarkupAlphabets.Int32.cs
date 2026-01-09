namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    public static class Int32
    {
        public static readonly MarkupAlphabet<int> Utf32 =
            new(1, ['<', '>', '/', ':', ' ', '"', '=',
                '!', '-', '[', ']', '?',
                'x', 'm', 'l', 'n', 's', 'i', 'd',
                '\'', '\r', '\n', '\t']);

        public static readonly MarkupAlphabet<int> Utf32BE =
            new(1, [1006632960, 1040187392, 788529152, 973078528, 536870912, 570425344, 1023410176,
                553648128, 754974720, 1526726656, 1560281088, 1056964608,
                2013265920, 1828716544, 1811939328, 1845493760, 1929379840, 1761607680, 1677721600,
                654311424, 218103808, 167772160, 150994944]);
    }
}