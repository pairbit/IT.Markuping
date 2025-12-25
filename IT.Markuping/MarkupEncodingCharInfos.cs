namespace IT.Markuping;

public static class MarkupEncodingCharInfos
{
    public static readonly MarkupEncodingInfo<char> Utf16_Strict =
        new(new(1, ['<', '>', '/', ':', ' ', '"', '=']),
            [1200]);

    public static readonly MarkupEncodingInfo<char> Utf16BE_Strict =
        new(new(1, [(char)15360, (char)15872, (char)12032, (char)14848, (char)8192, (char)8704, (char)15616]),
            [1201]);

    public static readonly MarkupEncodingInfo<char> Utf16 =
        new(new(1, ['<', '>', '/', ':', ' ', '"', '=',
            '\'', '\r', '\n', '\t']),
            [1200]);

    public static readonly MarkupEncodingInfo<char> Utf16BE =
        new(new(1, [(char)15360, (char)15872, (char)12032, (char)14848, (char)8192, (char)8704, (char)15616,
            (char)9984, (char)3328, (char)2560, (char)2304]),
            [1201]);
}