namespace IT.Markuping;

public static class MarkupEncodingInfos
{
    public static readonly MarkupEncodingInfo Utf8_Strict =
        new(new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'\'', (byte)'=']),
            [437, 708, 720, 737, 775, 850, 852, 855, 857, 858, 860, 861, 862, 863, 864, 865, 866, 869, 874, 932, 936, 949, 950, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1361, 10000, 10001, 10002, 10004, 10005, 10006, 10007, 10010, 10017, 10021, 10029, 10079, 10081, 10082, 20000, 20001, 20002, 20003, 20004, 20005, 20105, 20106, 20107, 20108, 20127, 20261, 20269, 20866, 20932, 20936, 20949, 21866, 28591, 28592, 28593, 28594, 28595, 28596, 28597, 28598, 28599, 28603, 28605, 29001, 65001]);

    public static readonly MarkupEncodingInfo EBCDIC_Strict =
        new(new(1, [76, 110, 97, 122, 64, 127, 125, 126]),
            [37, 500, 870, 875, 1047, 1140, 1141, 1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 20273, 20277, 20278, 20280, 20284, 20285, 20290, 20297, 20420, 20423, 20424, 20833, 20838, 20871, 20880, 20924, 21025]);

    public static readonly MarkupEncodingInfo EBCDIC_Turkish_Strict =
        new(new(1, [76, 110, 97, 122, 64, 252, 125, 126]),
            [1026, 20905]);

    public static readonly MarkupEncodingInfo Utf8 =
        new(new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'\'', (byte)'=',
            (byte)'\r', (byte)'\n', (byte)'\t']),
            [437, 708, 720, 737, 775, 850, 852, 855, 857, 858, 860, 861, 862, 863, 864, 865, 866, 869, 874, 932, 936, 949, 950, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1361, 10000, 10001, 10002, 10004, 10005, 10006, 10007, 10010, 10017, 10021, 10029, 10079, 10081, 10082, 20000, 20001, 20002, 20003, 20004, 20005, 20105, 20106, 20107, 20108, 20127, 20261, 20269, 20866, 20932, 20936, 20949, 21866, 28591, 28592, 28593, 28594, 28595, 28596, 28597, 28598, 28599, 28603, 28605, 65001]);

    public static readonly MarkupEncodingInfo Europa =
        new(new(1, [(byte)'<', (byte)'>', (byte)'/', (byte)':', (byte)' ', (byte)'"', (byte)'\'', (byte)'=',
            63, 63, 63]), [29001]);

    public static readonly MarkupEncodingInfo EBCDIC =
        new(new(1, [76, 110, 97, 122, 64, 127, 125, 126,
            13, 37, 5]),
            [37, 500, 870, 875, 1140, 1141, 1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 20273, 20277, 20278, 20280, 20284, 20285, 20290, 20297, 20420, 20423, 20424, 20833, 20838, 20871, 20880, 21025]);

    public static readonly MarkupEncodingInfo EBCDIC_Turkish =
        new(new(1, [76, 110, 97, 122, 64, 252, 125, 126,
            13, 37, 5]),
            [1026, 20905]);

    public static readonly MarkupEncodingInfo IBM_Latin1 =
        new(new(1, [76, 110, 97, 122, 64, 127, 125, 126,
            13, 21, 5]),
            [1047, 20924]);

    public static readonly MarkupEncodingInfo Utf16_Strict =
        new(new(2, [(byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'\'', 0, (byte)'=', 0]),
            [1200]);

    public static readonly MarkupEncodingInfo Utf16BE_Strict =
        new(new(2, [0, (byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'\'', 0, (byte)'=']),
            [1201]);

    public static readonly MarkupEncodingInfo Utf32_Strict =
        new(new(4, [(byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'\'', 0, 0, 0, (byte)'=', 0, 0, 0]),
            [12000]);

    public static readonly MarkupEncodingInfo Utf32BE_Strict =
        new(new(4, [0, 0, 0, (byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'\'', 0, 0, 0, (byte)'=']),
            [12001]);

    public static readonly MarkupEncodingInfo Utf16 =
        new(new(2, [(byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'\'', 0, (byte)'=', 0,
            (byte)'\r', 0, (byte)'\n', 0, (byte)'\t', 0]),
            [1200]);

    public static readonly MarkupEncodingInfo Utf16BE =
        new(new(2, [0, (byte)'<', 0, (byte)'>', 0, (byte)'/', 0, (byte)':', 0, (byte)' ', 0, (byte)'"', 0, (byte)'\'', 0, (byte)'=',
            0, (byte)'\r', 0, (byte)'\n', 0, (byte)'\t']),
            [1201]);

    public static readonly MarkupEncodingInfo Utf32 =
        new(new(4, [(byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'\'', 0, 0, 0, (byte)'=', 0, 0, 0,
            (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t', 0, 0, 0]),
            [12000]);

    public static readonly MarkupEncodingInfo Utf32BE =
        new(new(4, [0, 0, 0, (byte)'<', 0, 0, 0, (byte)'>', 0, 0, 0, (byte)'/', 0, 0, 0, (byte)':', 0, 0, 0, (byte)' ', 0, 0, 0, (byte)'"', 0, 0, 0, (byte)'\'', 0, 0, 0, (byte)'=',
            0, 0, 0, (byte)'\r', 0, 0, 0, (byte)'\n', 0, 0, 0, (byte)'\t']),
            [12001]);
}