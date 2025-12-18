using IT.Markuping.Implementation;
using IT.Markuping.Interfaces;
using System;
using System.Linq;
using System.Text;

namespace IT.Markuping.Tests;

internal class TagFinderByteTest
{
    [Test]
    public void Tester()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var encodingInfos = Encoding.GetEncodings();

        Console.WriteLine($"encodingInfos count: {encodingInfos.Length}");

        foreach (var encodingInfo in encodingInfos)
        {
            var codePage = encodingInfo.CodePage;

            if (TagFinderByte.TryGet(codePage, out var tagFinder))
            {
                var encoding = encodingInfo.GetEncoding();
                var tester = new TagFinderByteTester(tagFinder, encoding);

                tester.Test();
            }
            else
            {
                Assert.Fail($"{codePage,5} Options not supported");
            }
        }
    }

    [Test]
    public void FailTest()
    {
        var bytes = "------</Sig>98989<Sig>-----"u8;
        var name = "Sig"u8;

        bytes = "----<Sig/"u8;
        var opening = TagFinderByte.Utf8.Last(bytes, name);
        Assert.That(opening.IsEmpty, Is.True);

        bytes = Encoding.Unicode.GetBytes("------</Sig>98989<Sig>-----");
        name = Encoding.Unicode.GetBytes("Sig");
    }

    [Test]
    public void BaseTest()
    {
        var str = "<a /><a b='\"' c=\">\">123</a><a />";
        var nameStr = "a";

        var outerStr = "<a b='\"' c=\">\">123</a>";
        var innerStr = "123";

        var bytes = Encoding.UTF8.GetBytes(str);
        var name = Encoding.UTF8.GetBytes(nameStr);
        var outer = Encoding.UTF8.GetBytes(outerStr);
        var inner = Encoding.UTF8.GetBytes(innerStr);

        var last = TagFinderByte.Utf8.LastPair(bytes, name, out _);
        Assert.That(last.HasAttributes, Is.True);
        Assert.That(bytes[last.Outer].SequenceEqual(outer), Is.True);
        Assert.That(bytes[last.Inner].SequenceEqual(inner), Is.True);

        bytes = Encoding.Unicode.GetBytes(str);
        name = Encoding.Unicode.GetBytes(nameStr);
        outer = Encoding.Unicode.GetBytes(outerStr);
        inner = Encoding.Unicode.GetBytes(innerStr);

        //range = TagFinderBytes.Utf16.Outer(bytes, name);
        //Assert.That(bytes[range].SequenceEqual(outer), Is.True);

        //range = TagFinderBytes.Utf16.Inner(bytes, name);
        //Assert.That(bytes[range].SequenceEqual(inner), Is.True);
    }

    [Test]
    public void AllEncodings()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        var encodingInfos = Encoding.GetEncodings();

        Console.WriteLine($"encodingInfos count: {encodingInfos.Length}");

        foreach (var encodingInfo in encodingInfos)
        {
            var codePage = encodingInfo.CodePage;
            var encoding = encodingInfo.GetEncoding();

            //if (TagFinder.TryGetStrict(codePage, out var tagFinder))
            //{
            //    FullTest(tagFinder, encoding, "\0 ", false);
            //    FullTest(tagFinder, encoding, "\0 ", true);
            //}
            //else
            //{
            //    Console.WriteLine($"{codePage,5} Strict Options not supported");
            //}

            if (TagFinderByte.TryGet(codePage, out var tagFinder))
            {
                FullTest(tagFinder, encoding, "\0\t\n\r ", false);
                FullTest(tagFinder, encoding, "\0\t\n\r ", true);
            }
            else
            {
                Console.WriteLine($"{codePage,5} Options not supported");
            }
        }
    }

    private static void FullTest(ITagFinder<byte> tagFinder, Encoding encoding, string spaces, bool hasNamespace)
    {
        var nameBytes = encoding.GetBytes("Signature");

        foreach (var space in spaces)
        {
            try
            {
                var str = GetSignature2(space, hasNamespace);
                var bytes = encoding.GetBytes("Signature" + str + "Signature");
                var lastInner = "last";

                var last = tagFinder.LastPair(bytes, nameBytes, out _);
                Assert.That(last.HasNamespace, Is.EqualTo(hasNamespace));
                Assert.That(bytes[last.Outer].SequenceEqual(encoding.GetBytes(GetSignature(space, hasNamespace, lastInner))), Is.True);
                Assert.That(bytes[last.Inner].SequenceEqual(encoding.GetBytes(lastInner)), Is.True);
            }
            catch
            {
                Console.WriteLine($"CodePage: {encoding.CodePage}");
                Console.WriteLine($"space: '{space}'");
                Console.WriteLine($"hasNamespace: {hasNamespace}");
                throw;
            }
        }
    }

    private static string GetSignature2(char space, bool hasNamespace) =>
        GetSignature(space, hasNamespace, "first") +
        GetSignature(space, hasNamespace, "last");

    private static string GetSignature(char space, bool hasNamespace, string inner)
    {
        if (space == '\0') return hasNamespace
                ? $"<ds:Signature>{inner}</ds:Signature>"
                : $"<Signature>{inner}</Signature>";

        return hasNamespace
            ? $"<ds:Signature{space}a='\"/>' b=\"'>1\" c=3>{inner}</ds:Signature{space}>"
            : $"<Signature{space}a='\"/>' b=\"'>1\" c=3>{inner}</Signature{space}>";
    }
}