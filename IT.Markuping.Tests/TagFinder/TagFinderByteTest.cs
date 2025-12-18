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
    public void First()
    {
        FirstTest(TagFinderByte.Utf32, Encoding.UTF32);

        FirstTest(TagFinderByte.Utf16, Encoding.Unicode);

        FirstTest(TagFinderByte.Utf8, Encoding.UTF8);
    }

    [Test]
    public void LastClosing()
    {
        LastClosingTest(TagFinderByte.Utf32, Encoding.UTF32);

        LastClosingTest(TagFinderByte.Utf16, Encoding.Unicode);

        LastClosingTest(TagFinderByte.Utf8, Encoding.UTF8);
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

    private static void FirstTest(ITagFinder<byte> finder, Encoding encoding)
    {
        var name = encoding.GetBytes("a");
        var n = encoding.GetBytes("n");
        var bytes = encoding.GetBytes("<a/>").AsSpan();

        var opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("<a \r\n\t/>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        opening = finder.First(bytes, name, TagEndings.HasNoAttributes);
        Assert.That(opening.Unended, Is.EqualTo(TagEnding.Name));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes("<a ")), Is.True);

        bytes = encoding.GetBytes("<a\r\n\tb />");
        opening = finder.First(bytes, name, TagEndings.HasAttributes);
        Assert.That(opening.Unended, Is.EqualTo(TagEnding.AttributeStart));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes("<a\r\n\t")), Is.True);

        bytes = encoding.GetBytes("<a>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("<a \r\n\t>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("<a><a/></a><a b=3 /><a\rc=4></a>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes("<a>")), Is.True);

        opening = finder.First(bytes, name, TagEndings.SelfClosing);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes("<a/>")), Is.True);

        opening = finder.First(bytes, name, TagEndings.SelfClosingHasAttributes);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes("<a b=3 />")), Is.True);

        opening = finder.First(bytes, name, TagEndings.ClosingHasAttributes);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes("<a\rc=4>")), Is.True);

        bytes = encoding.GetBytes("<a b=\"'>\" c='\"/>'>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("<a b/>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("<a =b>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        //Fail
        Assert.That(finder.First(encoding.GetBytes("<a/"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a "), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a /"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b='"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b=''"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b='>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b='>'"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b=\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b=\"\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b=\">"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b=\">\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b=\"/>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a b=\"/>\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a '>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes("<a \">"), name).IsEmpty, Is.True);
    }

    private static void LastClosingTest(ITagFinder<byte> finder, Encoding encoding)
    {
        var name = encoding.GetBytes("a");
        var n = encoding.GetBytes("n");
        var bytes = encoding.GetBytes("</a>").AsSpan();

        var closing = finder.LastClosing(bytes, name, out var ns);
        Assert.That(bytes[ns].IsEmpty, Is.True);
        Assert.That(closing.HasNamespace, Is.False);
        Assert.That(closing.HasSpace, Is.False);
        Assert.That(bytes[closing.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("</a >");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(bytes[ns].IsEmpty, Is.True);
        Assert.That(closing.HasNamespace, Is.False);
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(bytes[closing.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("</a\n\r\t >");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(bytes[ns].IsEmpty, Is.True);
        Assert.That(closing.HasNamespace, Is.False);
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(bytes[closing.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("</n:a>");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(bytes[ns].SequenceEqual(n), Is.True);
        Assert.That(closing.HasNamespace, Is.True);
        Assert.That(closing.HasSpace, Is.False);
        Assert.That(bytes[closing.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("</n:a >");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(bytes[ns].SequenceEqual(n), Is.True);
        Assert.That(closing.HasNamespace, Is.True);
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(bytes[closing.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes("</n:a\n\r\t >");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(bytes[ns].SequenceEqual(n), Is.True);
        Assert.That(closing.HasNamespace, Is.True);
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(bytes[closing.Range].SequenceEqual(bytes), Is.True);

        //Fail
        bytes = encoding.GetBytes("</a\n\r\t b>");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        bytes = encoding.GetBytes("</a\n\r\t ");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        bytes = encoding.GetBytes("</n:a\n\r\t b>");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        bytes = encoding.GetBytes("</n:a\n\r\t ");
        closing = finder.LastClosing(bytes, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));
    }
}