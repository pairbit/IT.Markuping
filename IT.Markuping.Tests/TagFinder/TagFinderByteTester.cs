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
        var tagFullName = hasNamespace ? tagNamespace + ":" + tagName : tagName;
        var name = encoding.GetBytes(tagFullName);

        var data = encoding.GetBytes($"<{tagFullName}/>").AsSpan();
        var tag = finder.First(data, name);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"<{tagFullName} \r\n\t/>");
        tag = finder.First(data, name);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        tag = finder.First(data, name, TagEndings.HasNoAttributes);
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.Name));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName} ")), Is.True);

        data = encoding.GetBytes($"<{tagFullName}\r\n\tb />");
        tag = finder.First(data, name, TagEndings.HasAttributes);
        Assert.That(tag.Unended, Is.EqualTo(TagEnding.AttributeStart));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}\r\n\t")), Is.True);

        data = encoding.GetBytes($"<{tagFullName}>");
        tag = finder.First(data, name);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"<{tagFullName} \r\n\t>");
        tag = finder.First(data, name);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"<{tagFullName}><{tagFullName}/></{tagFullName}><{tagFullName} b=3 /><{tagFullName}\rc=4></{tagFullName}>");
        tag = finder.First(data, name);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}>")), Is.True);

        tag = finder.First(data, name, TagEndings.SelfClosing);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}/>")), Is.True);

        tag = finder.First(data, name, TagEndings.SelfClosingHasAttributes);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName} b=3 />")), Is.True);

        tag = finder.First(data, name, TagEndings.ClosingHasAttributes);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}\rc=4>")), Is.True);

        data = encoding.GetBytes($"<{tagFullName} b=\"'>\" c='\"/>'>");
        tag = finder.First(data, name);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"<{tagFullName} b/>");
        tag = finder.First(data, name);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"<{tagFullName} =b>");
        tag = finder.First(data, name);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        //Fail
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName}/"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} "), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} /"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b='"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=''"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b='>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b='>'"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\"\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\">"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\">\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\"/>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\"/>\""), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} '>"), name).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} \">"), name).IsEmpty, Is.True);
    }

    public void LastClosingTest(string tagName, string tagNamespace)
    {
        var encoding = _encoding;
        var finder = _finder;

        var hasNamespace = tagNamespace.Length > 0;
        var tagNS = encoding.GetBytes(tagNamespace);
        var name = encoding.GetBytes(tagName);
        var tagFullName = hasNamespace ? tagNamespace + ":" + tagName : tagName;

        var data = encoding.GetBytes($"</{tagFullName}>").AsSpan();
        var closing = finder.LastClosing(data, name, out var ns);
        Assert.That(data[ns].SequenceEqual(tagNS), Is.True);
        Assert.That(closing.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(closing.HasSpace, Is.False);
        Assert.That(data[closing.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"</{tagFullName} >");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(data[ns].SequenceEqual(tagNS), Is.True);
        Assert.That(closing.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(data[closing.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"</{tagFullName}\n\r\t >");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(data[ns].SequenceEqual(tagNS), Is.True);
        Assert.That(closing.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(closing.HasSpace, Is.True);
        Assert.That(data[closing.Range].SequenceEqual(data), Is.True);

        if (hasNamespace)
        {
            data = encoding.GetBytes($"</{tagFullName}>");
            closing = finder.LastClosing(data, encoding.GetBytes(tagFullName), out ns);
            Assert.That(data[ns].IsEmpty, Is.True);
            Assert.That(closing.HasNamespace, Is.False);
            Assert.That(closing.HasSpace, Is.False);
            Assert.That(data[closing.Range].SequenceEqual(data), Is.True);
        }

        //Fail
        data = encoding.GetBytes($"</{tagFullName}\n\r\t b>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"</{tagFullName}\n\r\t ");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"/{tagFullName}>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"<{tagFullName}>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes($"{tagFullName}>");
        closing = finder.LastClosing(data, name, out ns);
        Assert.That(closing.IsEmpty, Is.True);
        Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

        data = encoding.GetBytes(tagFullName);
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