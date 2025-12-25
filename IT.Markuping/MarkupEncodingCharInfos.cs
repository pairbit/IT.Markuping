namespace IT.Markuping;

public static class MarkupEncodingCharInfos
{
    public static readonly MarkupEncodingInfo<char> Utf16_Strict =
        new(new(1, ['<', '>', '/', ':', ' ', '"', '=']),
            [1200]);

    public static readonly MarkupEncodingInfo<char> Utf16 =
        new(new(1, ['<', '>', '/', ':', ' ', '"', '=',
            '\'', '\r', '\n', '\t']),
            [1200]);
}