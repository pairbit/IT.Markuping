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
        LastClosingTest("a", string.Empty);
        LastClosingTest("a", "n");

        FirstTest("a", string.Empty);
        FirstTest("a", "n");

        OldLastPairTest(false);
        OldLastPairTest(true);
    }

    public void FirstTest(string tagName, string tagNamespace)
    {
        var encoding = _encoding;
        var finder = _finder;

        var hasNamespace = tagNamespace.Length > 0;
        var tag = hasNamespace ? tagNamespace + ":" + tagName : tagName;
        var name = encoding.GetBytes(tag);

        var bytes = encoding.GetBytes($"<{tag}/>").AsSpan();
        var opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes($"<{tag} \r\n\t/>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        opening = finder.First(bytes, name, TagEndings.HasNoAttributes);
        Assert.That(opening.Unended, Is.EqualTo(TagEnding.Name));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes($"<{tag} ")), Is.True);

        bytes = encoding.GetBytes($"<{tag}\r\n\tb />");
        opening = finder.First(bytes, name, TagEndings.HasAttributes);
        Assert.That(opening.Unended, Is.EqualTo(TagEnding.AttributeStart));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes($"<{tag}\r\n\t")), Is.True);

        bytes = encoding.GetBytes($"<{tag}>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes($"<{tag} \r\n\t>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes($"<{tag}><{tag}/></{tag}><{tag} b=3 /><{tag}\rc=4></{tag}>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes($"<{tag}>")), Is.True);

        opening = finder.First(bytes, name, TagEndings.SelfClosing);
        Assert.That(opening.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes($"<{tag}/>")), Is.True);

        opening = finder.First(bytes, name, TagEndings.SelfClosingHasAttributes);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes($"<{tag} b=3 />")), Is.True);

        opening = finder.First(bytes, name, TagEndings.ClosingHasAttributes);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(encoding.GetBytes($"<{tag}\rc=4>")), Is.True);

        bytes = encoding.GetBytes($"<{tag} b=\"'>\" c='\"/>'>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes($"<{tag} b/>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        bytes = encoding.GetBytes($"<{tag} =b>");
        opening = finder.First(bytes, name);
        Assert.That(opening.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(bytes[opening.Range].SequenceEqual(bytes), Is.True);

        //Fail
        Assert.That(finder.First(encoding.GetBytes($"<{tag}/"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} "), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} /"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b='"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b=''"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b='>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b='>'"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b=\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b=\"\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b=\">"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b=\">\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b=\"/>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} b=\"/>\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} '>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tag} \">"), name).IsEmpty, Is.True);
    }

    public void LastClosingTest(string tagName, string tagNamespace)
    {
        var encoding = _encoding;
        var finder = _finder;

        var hasNamespace = tagNamespace.Length > 0;
        var tagNS = encoding.GetBytes(tagNamespace);
        var name = encoding.GetBytes(tagName);
        var tag = hasNamespace ? tagNamespace + ":" + tagName : tagName;

        var data = encoding.GetBytes($"</{tag}>").AsSpan();
        var closing = finder.LastClosing(data, name, out var ns);
        Assert.That(data[ns].SequenceEqual(tagNS), Is.True);
        Assert.That(closing.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(closing.HasSpace, Is.False);
        Assert.That(data[closing.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"</{tag} >");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(data[ns].SequenceEqual(tagNS), Is.True);
        Assert.That(closing.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(data[closing.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"</{tag}\n\r\t >");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(data[ns].SequenceEqual(tagNS), Is.True);
        Assert.That(closing.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(data[closing.Range].SequenceEqual(data), Is.True);

        if (hasNamespace)
        {
            data = encoding.GetBytes($"</{tag}>");
            closing = finder.LastClosing(data, encoding.GetBytes(tag), out ns);
            Assert.That(data[ns].IsEmpty, Is.True);
            Assert.That(closing.HasNamespace, Is.False);
            Assert.That(closing.HasSpace, Is.False);
            Assert.That(data[closing.Range].SequenceEqual(data), Is.True);
        }

        //Fail
        data = encoding.GetBytes($"</{tag}\n\r\t b>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"</{tag}\n\r\t ");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"/{tag}>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"<{tag}>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"{tag}>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes(tag);
        closing = finder.LastClosing(data, name, out ns);
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