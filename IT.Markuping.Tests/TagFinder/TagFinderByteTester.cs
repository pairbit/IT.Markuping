using IT.Markuping.Extensions;
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
        var fullName = encoding.GetBytes(tagFullName);
        var name = hasNamespace ? encoding.GetBytes(tagName) : fullName;
        var ns = encoding.GetBytes(tagNamespace);

        var endingName = encoding.GetBytes($"<{tagFullName} ");
        var endingName2 = encoding.GetBytes($"<{tagFullName}\r");
        var attributeStart = encoding.GetBytes($"<{tagFullName}\r\n\t");

        FirstTest($"<{tagFullName}>", fullName, name, ns, TagEnding.Closing);
        FirstTest($"<{tagFullName} \r\n\t>", fullName, name, ns, TagEnding.Closing, 
            endingName);
        FirstTest($"<{tagFullName} b>", fullName, name, ns, TagEnding.ClosingHasAttributes,
            endingName, endingName);
        FirstTest($"<{tagFullName}\rb>", fullName, name, ns, TagEnding.ClosingHasAttributes,
            endingName2, endingName2);
        FirstTest($"<{tagFullName}\r\n\tb >", fullName, name, ns, TagEnding.ClosingHasAttributes, 
            endingName2, attributeStart);

        FirstTest($"<{tagFullName}/>", fullName, name, ns, TagEnding.SelfClosing);
        FirstTest($"<{tagFullName} \r\n\t/>", fullName, name, ns, TagEnding.SelfClosing, 
            endingName);
        FirstTest($"<{tagFullName} b />", fullName, name, ns, TagEnding.SelfClosingHasAttributes,
            endingName, endingName);
        FirstTest($"<{tagFullName}\rb />", fullName, name, ns, TagEnding.SelfClosingHasAttributes,
            endingName2, endingName2);
        FirstTest($"<{tagFullName}\r\n\tb />", fullName, name, ns, TagEnding.SelfClosingHasAttributes,
            endingName2, attributeStart);

        var data = encoding.GetBytes($"<{tagFullName}><{tagFullName}/></{tagFullName}><{tagFullName} b=3 /><{tagFullName}\rc=4></{tagFullName}>");
        var tag = finder.First(data, fullName);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.Closing));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}>")), Is.True);

        tag = finder.First(data, fullName, TagEndings.SelfClosing);
        Assert.That(tag.Ending, Is.EqualTo(TagEnding.SelfClosing));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}/>")), Is.True);

        tag = finder.First(data, fullName, TagEndings.SelfClosingHasAttributes);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName} b=3 />")), Is.True);

        tag = finder.First(data, fullName, TagEndings.ClosingHasAttributes);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}\rc=4>")), Is.True);

        data = encoding.GetBytes($"<{tagFullName} b=\"'>\" c='\"/>'>");
        tag = finder.First(data, fullName);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"<{tagFullName} b/>");
        tag = finder.First(data, fullName);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.SelfClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        data = encoding.GetBytes($"<{tagFullName} =b>");
        tag = finder.First(data, fullName);
        Assert.That(tag.Ended, Is.EqualTo(TagEnding.ClosingHasAttributes));
        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        //Fail
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName}/"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} "), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} /"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b='"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=''"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b='>"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b='>'"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\""), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\"\""), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\">"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\">\""), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\"/>"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} b=\"/>\""), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} '>"), fullName).IsEmpty, Is.True);
        Assert.That(finder.First(encoding.GetBytes($"<{tagFullName} \">"), fullName).IsEmpty, Is.True);
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

    private Tag FirstTest(string data, ReadOnlySpan<byte> fullName,
        ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        return FirstTest(_encoding.GetBytes(data), fullName, name, ns, ending, endingName, attributeStart);
    }

    private Tag FirstTest(ReadOnlySpan<byte> data, ReadOnlySpan<byte> fullName,
        ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var availableEndings = GetAvailableEndings(ending);

        var endings = availableEndings[0];
        var tag = _finder.First(data, fullName, endings);

        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        if (ending.IsAnyClosingHasNoAttributes())
        {
            Assert.That(tag.Ending, Is.EqualTo(ending));
        }
        else if (ending.IsAnyClosingHasAttributes())
        {
            Assert.That(tag.Ended, Is.EqualTo(ending));
        }
        else
        {
            Assert.Fail($"{ending} not support");
        }

        Assert.That(_finder.First(data, name, ns, endings), Is.EqualTo(tag));

        for (int i = 1; i < availableEndings.Length; i++)
        {
            endings = availableEndings[i];
            Assert.That(_finder.First(data, fullName, endings), Is.EqualTo(tag));
            Assert.That(_finder.First(data, name, ns, endings), Is.EqualTo(tag));
        }

        foreach (var notAvailableEndings in GetNotAvailableEndings(ending))
        {
            Assert.That(_finder.First(data, fullName, notAvailableEndings).IsEmpty, Is.True);
            Assert.That(_finder.First(data, name, ns, notAvailableEndings).IsEmpty, Is.True);
        }

        if (endingName.IsEmpty)
        {
            Assert.That(_finder.First(data, fullName, TagEndings.HasNoAttributes), Is.EqualTo(tag));
            Assert.That(_finder.First(data, name, ns, TagEndings.HasNoAttributes), Is.EqualTo(tag));
        }
        else
        {
            var tag2 = _finder.First(data, fullName, TagEndings.HasNoAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.Name));
            Assert.That(data[tag2.Range].SequenceEqual(endingName), Is.True);

            Assert.That(_finder.First(data, name, ns, TagEndings.HasNoAttributes), Is.EqualTo(tag2));
        }

        //Без атрибутов
        if (attributeStart.IsEmpty)
        {
            //Без пробелов
            if (endingName.IsEmpty)
            {
                Assert.That(_finder.First(data, fullName, TagEndings.HasAttributes).IsEmpty, Is.True);
                Assert.That(_finder.First(data, name, ns, TagEndings.HasAttributes).IsEmpty, Is.True);
            }
            else
            {
                Assert.That(_finder.First(data, fullName, TagEndings.HasAttributes), Is.EqualTo(tag));
                Assert.That(_finder.First(data, name, ns, TagEndings.HasAttributes), Is.EqualTo(tag));
            }
        }
        else
        {
            var tag2 = _finder.First(data, fullName, TagEndings.HasAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.AttributeStart));
            Assert.That(data[tag2.Range].SequenceEqual(attributeStart), Is.True);

            Assert.That(_finder.First(data, name, ns, TagEndings.HasAttributes), Is.EqualTo(tag2));
        }

        return tag;
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

    private static TagEndings[] GetAvailableEndings(TagEnding ending)
    {
        return ending switch
        {
            TagEnding.Closing => Closing_Available_Endings,
            TagEnding.SelfClosing => SelfClosing_Available_Endings,
            TagEnding.ClosingHasAttributes => ClosingHasAttributes_Available_Endings,
            TagEnding.SelfClosingHasAttributes => SelfClosingHasAttributes_Available_Endings,
            _ => throw new NotImplementedException(),
        };
    }

    private static TagEndings[] GetNotAvailableEndings(TagEnding ending)
    {
        return ending switch
        {
            TagEnding.Closing => Closing_NotAvailable_Endings,
            TagEnding.SelfClosing => SelfClosing_NotAvailable_Endings,
            TagEnding.ClosingHasAttributes => ClosingHasAttributes_NotAvailable_Endings,
            TagEnding.SelfClosingHasAttributes => SelfClosingHasAttributes_NotAvailable_Endings,
            _ => throw new NotImplementedException(),
        };
    }

    private static readonly TagEndings[] Closing_Available_Endings = [
        TagEndings.AnyClosing, TagEndings.AnyClosingHasNoAttributes,
        TagEndings.Closing, TagEndings.ClosingHasNoAttributes];

    private static readonly TagEndings[] ClosingHasAttributes_Available_Endings = [
        TagEndings.AnyClosing, TagEndings.AnyClosingHasAttributes,
        TagEndings.Closing, TagEndings.ClosingHasAttributes];

    private static readonly TagEndings[] SelfClosing_Available_Endings = [
        TagEndings.AnyClosing, TagEndings.AnyClosingHasNoAttributes,
        TagEndings.SelfClosing, TagEndings.SelfClosingHasNoAttributes];

    private static readonly TagEndings[] SelfClosingHasAttributes_Available_Endings = [
        TagEndings.AnyClosing, TagEndings.AnyClosingHasAttributes,
        TagEndings.SelfClosing, TagEndings.SelfClosingHasAttributes];

    private static readonly TagEndings[] Closing_NotAvailable_Endings = [
        TagEndings.AnyClosingHasAttributes, TagEndings.ClosingHasAttributes,
        TagEndings.SelfClosing, TagEndings.SelfClosingHasAttributes,
        TagEndings.SelfClosingHasNoAttributes
    ];

    private static readonly TagEndings[] ClosingHasAttributes_NotAvailable_Endings = [
        TagEndings.AnyClosingHasNoAttributes, TagEndings.ClosingHasNoAttributes,
        TagEndings.SelfClosing, TagEndings.SelfClosingHasAttributes,
        TagEndings.SelfClosingHasNoAttributes
    ];

    private static readonly TagEndings[] SelfClosing_NotAvailable_Endings = [
        TagEndings.AnyClosingHasAttributes, TagEndings.SelfClosingHasAttributes,
        TagEndings.Closing, TagEndings.ClosingHasAttributes,
        TagEndings.ClosingHasNoAttributes
    ];

    private static readonly TagEndings[] SelfClosingHasAttributes_NotAvailable_Endings = [
        TagEndings.AnyClosingHasNoAttributes, TagEndings.SelfClosingHasNoAttributes,
        TagEndings.Closing, TagEndings.ClosingHasAttributes,
        TagEndings.ClosingHasNoAttributes
    ];
}