using System;
using System.Text;

namespace IT.Markuping.Tests;

internal class BOMTest
{
    [Test]
    public void Test()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var encodingInfos = Encoding.GetEncodings();

        Console.WriteLine($"encodingInfos count: {encodingInfos.Length}");

        foreach (var encodingInfo in encodingInfos)
        {
            var codePage = encodingInfo.CodePage;
            var encoding = encodingInfo.GetEncoding();

            try
            {
                var bom = GetBOM(codePage);

                var preamble = encoding.GetPreamble();
                if (preamble.Length > 0)
                {
                    Assert.That(GetBOM(preamble), Is.EqualTo(bom));
                }

                Assert.That(GetBOM(encoding.GetBytes("<a></a>")), Is.EqualTo(bom));
                Assert.That(GetBOM(encoding.GetBytes("<?xm")), Is.EqualTo(bom));
                Assert.That(GetBOM(encoding.GetBytes("<")), Is.EqualTo(bom));
            }
            catch
            {
                Console.WriteLine(codePage);
                throw;
            }
        }
    }

    private static readonly int[] EBCDIC_CodePages = [37, 500, 870, 875, 1026, 1047, 1140, 1141, 1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 20273, 20277, 20278, 20280, 20284, 20285, 20290, 20297, 20420, 20423, 20424, 20833, 20838, 20871, 20880, 20905, 20924, 21025];

    //<?xm
    private static readonly byte[] Decl_Utf8 = [60, 63, 120, 109];
    private static readonly byte[] Decl_EBCDIC = [76, 111, 167, 148];
    private static readonly byte[] Decl_EBCDIC_Japanese_katakana = [76, 111, 183, 117];

    //<
    private static readonly byte LT_Utf8 = 60;
    private static readonly byte LT_EBCDIC = 76;

    private static BOM GetBOM(int codePage)
    {
        if (codePage == 65000) return BOM.None;
        if (codePage == 1200) return BOM.Utf16;
        if (codePage == 1201) return BOM.Utf16BE;

        if (codePage == 12000) return BOM.Utf32;
        if (codePage == 12001) return BOM.Utf32BE;
        //if (codePage == 20290) return BOM.EBCDIC_Japanese_katakana;

        if (EBCDIC_CodePages.AsSpan().IndexOf(codePage) > -1)
            return BOM.EBCDIC;

        return BOM.Utf8;
    }

    private static BOM GetBOM(ReadOnlySpan<byte> xml)
    {
        var len = xml.Length;
        if (len > 0)
        {
            var first = xml[0];
            if (first == 0)
            {
                if (len > 1)
                {
                    var second = xml[1];
                    if (second == 0)
                    {
                        if (len > 3)
                        {
                            var third = xml[2];
                            if (third == 254)
                            {
                                var fourth = xml[3];
                                if (fourth == 255)
                                    return BOM.Utf32BE;

                                return BOM.None;
                            }
                            if (third == 0)
                            {
                                var fourth = xml[3];
                                if (fourth == 60)
                                    return BOM.Utf32BE;

                                return BOM.None;
                            }
                        }
                        return BOM.None;
                    }

                    if (second == 60)
                        return BOM.Utf16BE;
                }
                return BOM.None;
            }

            if (first == LT_Utf8)
            {
                if (len > 1)
                {
                    var second = xml[1];
                    if (second == 0)
                    {
                        if (len > 3 && xml[2] == 0 && xml[3] == 0)
                            return BOM.Utf32;

                        return BOM.Utf16;
                    }
                }
                return BOM.Utf8;
            }

            if (first == LT_EBCDIC)
            {
                //if (len > 3)
                //{
                    //76, 111, 183, 117
                    //if (xml[1] == 111 && xml[2] == 183 && xml[3] == 117)
                    //    return BOM.EBCDIC_Japanese_katakana;
                //}
                return BOM.EBCDIC;
            }

            if (first == 239)
            {
                if (len > 2)
                {
                    //239, 187, 191
                    if (xml[1] == 187 && xml[2] == 191)
                        return BOM.Utf8;
                }
                return BOM.None;
            }

            if (first == 254)
            {
                if (len > 1)
                {
                    //254, 255
                    if (xml[1] == 255)
                        return BOM.Utf16BE;
                }
                return BOM.None;
            }

            if (first == 255)
            {
                if (len > 1)
                {
                    var second = xml[1];
                    if (second == 254)
                    {
                        if (len > 3)
                        {
                            if (xml[2] == 0 && xml[3] == 0)
                                return BOM.Utf32;
                        }
                        return BOM.Utf16;
                    }
                }
                return BOM.None;
            }
        }
        return BOM.None;
    }

    enum BOM
    {
        None = 0,
        Utf7,
        Utf8,
        Utf16,
        Utf16BE,
        Utf32,
        Utf32BE,
        EBCDIC,
        //20290
        //EBCDIC_Japanese_katakana
    }
}