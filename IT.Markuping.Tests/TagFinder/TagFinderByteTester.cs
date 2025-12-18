using IT.Markuping.Interfaces;
using System;
using System.Linq;
using System.Text;

namespace IT.Markuping.Tests;

internal class TagFinderByteTester
{
    private readonly ITagFinder<byte> _finder;
    private readonly Encoding _encoding;

    public TagFinderByteTester(ITagFinder<byte> finder, Encoding encoding)
    {
        _finder = finder;
        _encoding = encoding;
    }

    public void Test()
    {
        LastClosingTest();
        FirstTest();

        OldLastPairTest(false);
        OldLastPairTest(true);
    }

    public void FirstTest()
    {
        var encoding = _encoding;
        var finder = _finder;

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

    public void LastClosingTest()
    {
        var encoding = _encoding;
        var finder = _finder;

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

    private void OldLastPairTest(bool hasNamespace)
    {
        var encoding = _encoding;
        var finder = _finder;

        var nameBytes = encoding.GetBytes("Signature");

        foreach (var space in "\0\t\n\r ")
        {
            try
            {
                var str = GetSignature2(space, hasNamespace);
                var bytes = encoding.GetBytes("Signature" + str + "Signature");
                var lastInner = "last";

                var last = finder.LastPair(bytes, nameBytes, out _);
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