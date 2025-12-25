using System;
using System.Collections.Generic;
using System.Text;

namespace IT.Markuping.Tests;

internal class SystemTextEncodingTest
{
    [Test]
    public void Xml_GetEncodings_Test()
    {
        var abc = "<>/: \"'=!-[]?\r\n\t";
        var arrayUtf8 = Encoding.UTF8.GetBytes(abc);
        //var arrayUtf7 = Encoding.UTF7.GetBytes(abc);

        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var array20880 = Encoding.GetEncoding(20880).GetBytes(abc);
        var array20905 = Encoding.GetEncoding(20905).GetBytes(abc);

        //Console.WriteLine($"UTF7: {string.Join(",", arrayUtf7)}");
        //Console.WriteLine($"UTF8: {string.Join(",", arrayUtf8)}");
        //Console.WriteLine($"20880: {string.Join(",", array20880)} = {new string(array20880.Select(x => (char)x).ToArray())}");
        //Console.WriteLine($"20905: {string.Join(",", array20905)}");

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
}