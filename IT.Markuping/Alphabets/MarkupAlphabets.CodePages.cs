namespace IT.Markuping;

public static partial class MarkupAlphabets
{
    internal static class CodePages
    {
        public static readonly int[] Utf8 = [437, 708, 720, 737, 775, 850, 852, 855, 857, 858, 860, 861, 862, 863, 864, 865, 866, 869, 874, 932, 936, 949, 950, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1361, 10000, 10001, 10002, 10003, 10004, 10005, 10006, 10007, 10008, 10010, 10017, 10021, 10029, 10079, 10081, 10082, 20000, 20001, 20002, 20003, 20004, 20005, 20105, 20127, 20261, 20269, 20866, 20932, 20936, 20949, 21866, 28591, 28592, 28593, 28594, 28595, 28596, 28597, 28598, 28599, 28603, 28605, 38598, 50220, 50221, 50222, 50225, 50227, 51932, 51936, 51949, 52936, 54936, 57002, 57003, 57004, 57005, 57006, 57007, 57008, 57009, 57010, 57011, 65001];
        public static readonly int[] IA5 = [20106, 20107, 20108];
        public const int Europa = 29001;

        public static readonly int[] EBCDIC = [500, 870, 875, 1148, 20423, 20880, 21025];
        public static readonly int[] EBCDIC_Canada_Hebrew = [37, 1140, 20424];
        public static readonly int[] EBCDIC_IBM_Latin1 = [1047, 20924];
        public static readonly int[] EBCDIC_Germany = [1141, 20273];
        public static readonly int[] EBCDIC_Denmark_Norway = [1142, 20277];
        public static readonly int[] EBCDIC_Finland_Sweden = [1143, 20278];
        public static readonly int[] EBCDIC_Italy = [1144, 20280];
        public static readonly int[] EBCDIC_Spain = [1145, 20284];
        public static readonly int[] EBCDIC_UK = [1146, 20285];
        public static readonly int[] EBCDIC_France = [1147, 20297];
        public static readonly int[] EBCDIC_Icelandic = [1149, 20871];
        public const int EBCDIC_Turkish_Latin5 = 1026;
        public const int EBCDIC_Japanese_katakana = 20290;
        public const int EBCDIC_Arabic = 20420;
        public const int EBCDIC_Korean_Extended = 20833;
        public const int EBCDIC_Thai = 20838;
        public const int EBCDIC_Turkish = 20905;
    }
}