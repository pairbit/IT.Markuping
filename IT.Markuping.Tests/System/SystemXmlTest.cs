using System;
using System.Xml;

namespace IT.Markuping.Tests;

public class SystemXmlTest
{
    /// <summary>
    /// 9,10,13,32
    /// </summary>
    //[Test]
    public void LoadXml256Test()
    {
        for (int i = 0; i < 256; i++)
        {
            var xml = "<ds:tag" + (char)i + "xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"></ds:tag>";

            try
            {
                Assert.That(ToStrict(xml), Is.EqualTo("<ds:tag xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"></ds:tag>"));

                Console.Write($"{i},");
            }
            catch (XmlException)
            {

            }
        }
    }

    [Test]
    public void LoadXmlTest()
    {
        var abc = "\t\n\r ";
        foreach (char space in abc)
        {
            Assert.That(ToStrict("<ds:tag" + space + "xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"></ds:tag>"),
                Is.EqualTo("<ds:tag xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"></ds:tag>"));
        }
    }

    [Test]
    public void InvalidXml()
    {
        var ex = Assert.Throws<XmlException>(() => ToStrict("<a b=4 />"));
#if NET
        Assert.That(ex.Message, Is.EqualTo(
            "'4' is an unexpected token. The expected token is '\"' or '''. Line 1, position 6."));
#endif
        ex = Assert.Throws<XmlException>(() => ToStrict("<a b=\"\\\"\" />"));
#if NET
        Assert.That(ex.Message, Is.EqualTo(
            "Name cannot begin with the '\"' character, hexadecimal value 0x22. Line 1, position 9."));
#endif
        ex = Assert.Throws<XmlException>(() => ToStrict("<a b=\"<\" />"));
#if NET
        Assert.That(ex.Message, Is.EqualTo(
            "'<', hexadecimal value 0x3C, is an invalid attribute character. Line 1, position 7."));
#endif

        ex = Assert.Throws<XmlException>(()=>LoadXml(@"
<root>
    <my:Signature>1</my:Signature>
    <my:Signature xmlns:my='uri2'>2</my:Signature>
</root>
"));
#if NET
        Assert.That(ex.Message, Is.EqualTo(
            "'my' is an undeclared prefix. Line 3, position 6."));
#endif
    }

    [Test]
    public void StrictTest()
    {
        //''' to '"'
        Assert.That(ToStrict("<a b='c' />"), Is.EqualTo("<a b=\"c\" />"));

        //'"' to '&quot;'
        Assert.That(ToStrict("<a b='\"'></a>"), Is.EqualTo("<a b=\"&quot;\"></a>"));

        //'>' to '&gt;'
        Assert.That(ToStrict("<a a='>' b=\">\"></a>"),
            Is.EqualTo("<a a=\"&gt;\" b=\"&gt;\"></a>"));

        //trim ' \t\r\n'
        Assert.That(ToStrict("<a \t\r\n><a \t\r\n/></a \t\r\n>"),
            Is.EqualTo("<a><a /></a>"));

        //self closing
        Assert.That(ToStrict("<a \t\r\n/>"), Is.EqualTo("<a />"));
        Assert.That(ToStrict("<a/>"), Is.EqualTo("<a />"));

        //trim ' \t\r\n' attributes
        Assert.That(ToStrict("<a \t\r\n a=\"b\" \t\r\n ></a>"),
            Is.EqualTo("<a a=\"b\"></a>"));

        Assert.That(ToStrict("<a b='/ns:c>' />"), Is.EqualTo("<a b=\"/ns:c&gt;\" />"));

        Assert.That(ToStrict("<b c=\":a>\" />"), Is.EqualTo("<b c=\":a&gt;\" />"));
    }

    [Test]
    public void CommentsTest()
    {
        StrictTest("<!--<a>--><a><!--</a>--><!--<a>--></a><!--</a>-->");
        StrictTest("<a><![CDATA[<a></a>]]></a>");

        StrictTest(@"
<root xmlns:my=""uri1"" xmlns:other=""uri2"">
    <my:Signature>1</my:Signature>
    <other:Signature>2</other:Signature>
</root>
");

        //Данный пример доказывает, что нельзя сначала искать NS prefix по URI
        //Потому что prefix могут перетираться разными значениями URI
        //Правильный алгоритм:
        //1. Сначала ищем тег по имени Signature;
        //2. Если есть ns prefix ищем его uri начиная с opening.End до начала документа
        //то-есть LastIndexOf. URI может быть объявляен как в самом теге атрибута,
        //так и вне его, но не ниже;
        StrictTest(@"
<root xmlns:my=""uri1"">
    <my:Signature>1</my:Signature>
    <my:Signature xmlns:my=""uri2"">2</my:Signature>
</root>
");
    }

    [Test]
    public void XPathTest()
    {
        var xml = LoadXml("<root><a><b><inner>inner text</inner></b></a></root>");

        Assert.That(xml.SelectSingleNode("//b")!.OuterXml, Is.EqualTo("<b><inner>inner text</inner></b>"));

        Assert.That(xml.SelectSingleNode("//b/node()")!.OuterXml, Is.EqualTo("<inner>inner text</inner>"));
    }

    private static void StrictTest(string xml, bool preserveWhitespace = true)
    {
        Assert.That(ToStrict(xml, preserveWhitespace), Is.EqualTo(xml));
    }

    private static string ToStrict(string xml, bool preserveWhitespace = true)
    {
        return LoadXml(xml, preserveWhitespace).OuterXml;
    }

    private static XmlDocument LoadXml(string xml, bool preserveWhitespace = true)
    {
        var doc = new XmlDocument() { PreserveWhitespace = preserveWhitespace };
        doc.LoadXml(xml);

        return doc;
    }
}