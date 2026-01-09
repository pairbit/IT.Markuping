using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IT.Markuping.Tests;

internal class SystemTextEncodingTest
{
    class EncodingMap
    {
        public List<int> CodePages { get; set; } = new();

        public byte[] Bytes { get; set; } = [];

        public bool IsSingle { get; set; }

        public bool HasDuplicates { get; set; }
    }

    //[Test]
    public void Xml_GetEncodings_Test()
    {
        //<>/: \"'=!-[]?\r\n\t
        //<>/: \"'=\r\n\t
        var abc = "<>/: \"'=\r\n\t";
        Console.WriteLine(abc);
        var arrayUtf8 = Encoding.UTF8.GetBytes(abc);
        //var arrayUtf7 = Encoding.UTF7.GetBytes(abc);

        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var array20880 = Encoding.GetEncoding(20880).GetBytes(abc);
        var array20905 = Encoding.GetEncoding(20905).GetBytes(abc);

        //Console.WriteLine($"UTF7: {string.Join(",", arrayUtf7)}");
        Console.WriteLine($"UTF8: {string.Join(",", arrayUtf8)}");
        Console.WriteLine($"20880: {string.Join(",", array20880)}");
        Console.WriteLine($"20905: {string.Join(",", array20905)}");

        var utf8s = new List<int>();
        var ibm20880s = new List<int>();
        var ibm20905s = new List<int>();
        var encodings = Encoding.GetEncodings();

        for (int i = 0; i < encodings.Length; i++)
        {
            var encodingInfo = encodings[i];
            var encoding = encodingInfo.GetEncoding();

            var bytes = encoding.GetBytes(abc);

            if (bytes.AsSpan().SequenceEqual(arrayUtf8))
            {
                utf8s.Add(encodingInfo.CodePage);
            }
            else if (bytes.AsSpan().SequenceEqual(array20880))
            {
                ibm20880s.Add(encodingInfo.CodePage);
            }
            else if (bytes.AsSpan().SequenceEqual(array20905))
            {
                ibm20905s.Add(encodingInfo.CodePage);
            }
            else
            {
                Console.WriteLine($"{encodingInfo.CodePage,5} | {encodingInfo.DisplayName,40} | {encodingInfo.Name,25} |{string.Join(",", bytes),20}");
            }
        }

        utf8s.Sort();
        ibm20880s.Sort();
        ibm20905s.Sort();

        Console.WriteLine($"\nUTF8: {string.Join(", ", utf8s)}");

        foreach (var codePage in utf8s)
        {
            var encoding = Encoding.GetEncoding(codePage);
            Console.WriteLine($"{codePage,5} | {encoding.EncodingName,40} | {encoding.WebName,25} | {encoding.HeaderName,25} | {encoding.BodyName,25}");
        }

        Console.WriteLine($"\n20880: {string.Join(", ", ibm20880s)}");
        foreach (var codePage in ibm20880s)
        {
            var encoding = Encoding.GetEncoding(codePage);
            Console.WriteLine($"{codePage,5} | {encoding.EncodingName,40} | {encoding.WebName,25} | {encoding.HeaderName,25} | {encoding.BodyName,25}");
        }

        Console.WriteLine($"\n20905: {string.Join(", ", ibm20905s)}");
        foreach (var codePage in ibm20905s)
        {
            var encoding = Encoding.GetEncoding(codePage);
            Console.WriteLine($"{codePage,5} | {encoding.EncodingName,40} | {encoding.WebName,25} | {encoding.HeaderName,25} | {encoding.BodyName,25}");
        }
    }
    
    [Test]
    public void UnknownTest()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var encoding = Encoding.GetEncoding(29001);
        Assert.That(encoding.WebName, Is.EqualTo("x-europa"));

        byte[] unknown = [63];
        Assert.That(encoding.GetBytes("?").SequenceEqual(unknown), Is.True);
        Assert.That(encoding.GetBytes("\r").SequenceEqual(unknown), Is.True);
        Assert.That(encoding.GetBytes("\n").SequenceEqual(unknown), Is.True);
        Assert.That(encoding.GetBytes("\t").SequenceEqual(unknown), Is.True);

        //x-ia5-
        for (int codePage = 20106; codePage <= 20108; codePage++)
        {
            encoding = Encoding.GetEncoding(codePage);
            Assert.That(encoding.WebName.StartsWith("x-ia5-"), Is.True);
            Assert.That(encoding.GetBytes("?").SequenceEqual(unknown), Is.True);
            Assert.That(encoding.GetBytes("[").SequenceEqual(unknown), Is.True);
            Assert.That(encoding.GetBytes("]").SequenceEqual(unknown), Is.True);
        }

        encoding = Encoding.GetEncoding(20420);
        Assert.That(encoding.EncodingName, Is.EqualTo("IBM EBCDIC (Arabic)"));
        unknown = [111];
        Assert.That(encoding.GetBytes("?").SequenceEqual(unknown), Is.True);
        Assert.That(encoding.GetBytes("[").SequenceEqual(unknown), Is.True);
        Assert.That(encoding.GetBytes("]").SequenceEqual(unknown), Is.True);
    }

    //[Test]
    public void Recode()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var enc = Encoding.GetEncoding(500);
        var bytes = enc.GetBytes($"<?xml version=\"1.0\" encoding=\"{enc.WebName}\"?><Doc><Field>Text</Field></Doc>");
        var base64 = Convert.ToBase64String(bytes);
    }

    //[Test]
    public void PreambleTest()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        var encodingInfos = Encoding.GetEncodings();
        foreach (var encodingInfo in encodingInfos)
        {
            var encoding = encodingInfo.GetEncoding();
            var codePage = encoding.CodePage;
            var preamble = encoding.GetPreamble();
            if (preamble.Length == 0) continue;
            var preambleStr = string.Join(", ", preamble);
            Console.WriteLine($"{preambleStr,10} | {codePage,5} | {encoding.EncodingName,40} | {encoding.WebName,25} | {encoding.HeaderName,25} | {encoding.BodyName,25}");
        }
    }

    [Test]
    public void MappingTest()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        //<>/: \"=!-[]?xmlns'\r\n\t
        //<>/: \"=!-[]?'\r\n\t
        //<>/: \"=!-?xmlns'\r\n\t (ok)
        //<>/: \"=xmlns'\r\n\t (ok)

        //<>/: \"='\r\n\t (2.0.4)
        //<>/: \"= (2.0.4 strict)
        //<>/: \"=!-[]?xmlnsid'\r\n\t (full 2.0.5)
        //<>/: \"=!-[]?xmlnsid (strict 2.0.5)
        var abc = "<>/: \"=!-[]?xmlnsid'\r\n\t";
        var maps = GetMaps(abc).OrderByDescending(x => x.CodePages.Count);

        Console.WriteLine($"Abc#{abc.Length}: '{abc}' ");
        foreach (var map in maps)
        {
            Console.WriteLine($"\nSingle: {map.IsSingle}, HasDuplicates: {map.HasDuplicates}");
            Console.WriteLine($"Bytes: {string.Join(", ", map.Bytes)}");
            Console.WriteLine($"CodePages: {string.Join(", ", map.CodePages)}");
            foreach (var codePage in map.CodePages)
            {
                var encoding = Encoding.GetEncoding(codePage);
                Console.WriteLine($"{codePage,5} | {encoding.EncodingName,40} | {encoding.WebName,25} | {encoding.HeaderName,25} | {encoding.BodyName,25}");
            }
        }
    }

    private static List<EncodingMap> GetMaps(string abc)
    {
        var maps = new List<EncodingMap>();
        var encodingInfos = Encoding.GetEncodings();
        foreach (var encodingInfo in encodingInfos)
        {
            var codePage = encodingInfo.CodePage;
            if (codePage == 1200 || codePage == 1201 ||
                codePage == 12000 || codePage == 12001 ||
                codePage == 65000) continue;

            var encoding = encodingInfo.GetEncoding();
            var bytes = encoding.GetBytes(abc);
            if (TryFindMap(maps, bytes, out var map))
            {
                map.CodePages.Add(codePage);
            }
            else
            {
                maps.Add(new()
                {
                    Bytes = bytes,
                    CodePages = new() { codePage },
                    IsSingle = bytes.Length == abc.Length,
                    HasDuplicates = bytes.Distinct().Count() != abc.Length
                });
            }
        }

        foreach (var map in maps)
        {
            map.CodePages.Sort();
        }

        return maps;
    }

    private static bool TryFindMap(IList<EncodingMap> maps, byte[] bytes,
#if NET
        [System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)]
#endif
    out EncodingMap map)
    {
        for (int i = 0; i < maps.Count; i++)
        {
            map = maps[i];
            if (map.Bytes.SequenceEqual(bytes))
            {
                return true;
            }
        }
        map = null
#if !NET
        !
#endif
            ;
        return false;
    }
}