namespace IT.Markuping;

public static class MarkupEncodingIntInfos
{
    public static readonly MarkupEncodingInfo<int> Utf32_Strict =
        new(new(1, ['<', '>', '/', ':', ' ', '"', '=']),
            [12000]);

    public static readonly MarkupEncodingInfo<int> Utf32BE_Strict =
        new(new(1, [1006632960, 1040187392, 788529152, 973078528, 536870912, 570425344, 1023410176]),
            [12001]);

    public static readonly MarkupEncodingInfo<int> Utf32 =
        new(new(1, ['<', '>', '/', ':', ' ', '"', '=',
            '\'', '\r', '\n', '\t']),
            [12000]);

    public static readonly MarkupEncodingInfo<int> Utf32BE =
        new(new(1, [1006632960, 1040187392, 788529152, 973078528, 536870912, 570425344, 1023410176,
            654311424, 218103808, 167772160, 150994944]),
            [12001]);
}