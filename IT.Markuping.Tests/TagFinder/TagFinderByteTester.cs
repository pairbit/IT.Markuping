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

    public class TagData
    {
        public string Name { get; }

        public string NameSpace { get; }

        public string FullName { get; }

        public byte[] NameBytes { get; }

        public byte[] NameSpaceBytes { get; }

        public byte[] FullNameBytes { get; }

        public TagData(Encoding encoding, string name)
        {
            FullName = Name = name;
            FullNameBytes = NameBytes = encoding.GetBytes(name);
            NameSpace = string.Empty;
            NameSpaceBytes = [];
        }

        public TagData(Encoding encoding, string name, string nameSpace)
        {
            Name = name;
            NameSpace = nameSpace;
            FullName = nameSpace + ":" + name;
            NameBytes = encoding.GetBytes(name);
            NameSpaceBytes = encoding.GetBytes(nameSpace);
            FullNameBytes = encoding.GetBytes(FullName);
        }

        public override string ToString() => FullName;
    }

    public void Test()
    {
        Test(new(_encoding, "a"));
        Test(new(_encoding, "a", "n"));

        OldLastPairTest(false);
        OldLastPairTest(true);
    }

    public void Test(TagData tagData)
    {
        LastClosingTest(tagData);

        FirstLastTest(tagData);

        Pairs(tagData);
    }

    public void Pairs(TagData tagData)
    {
        var tagFullName = tagData.FullName;
        var hasNamespace = tagData.NameSpace.Length > 0;
        var name = tagData.NameBytes;
        
        var data = _encoding.GetBytes($"<{tagFullName}></{tagFullName}>").AsSpan();
        var last = _finder.LastPair(data, name, out var ns);
        Assert.That(last.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(data[ns].SequenceEqual(tagData.NameSpaceBytes), Is.True);
        Assert.That(data[last.Outer].SequenceEqual(data), Is.True);
        Assert.That(data[last.Inner].IsEmpty, Is.True);

        data = _encoding.GetBytes($"<{tagFullName}>first</{tagFullName}><{tagFullName}>last</{tagFullName}>").AsSpan();
        last = _finder.LastPair(data, name, out ns);
        Assert.That(last.HasNamespace, Is.EqualTo(hasNamespace));
        Assert.That(data[ns].SequenceEqual(tagData.NameSpaceBytes), Is.True);
        Assert.That(data[last.Outer].SequenceEqual(_encoding.GetBytes($"<{tagFullName}>last</{tagFullName}>")), Is.True);
        Assert.That(data[last.Inner].SequenceEqual(_encoding.GetBytes("last")), Is.True);
    }

    public void FirstLastTest(TagData tagData)
    {
        var tagFullName = tagData.FullName;
        var fullName = tagData.FullNameBytes;
        var name = tagData.NameBytes;
        var ns = tagData.NameSpaceBytes;

        var endingName = _encoding.GetBytes($"<{tagFullName} ");
        var endingName2 = _encoding.GetBytes($"<{tagFullName}\r");
        var attributeStart = _encoding.GetBytes($"<{tagFullName}\r\n\t");

        FirstLastTest($"<{tagFullName}>", fullName, name, ns, TagEnding.Closing);
        FirstLastTest($"<{tagFullName} \r\n\t>", fullName, name, ns, TagEnding.Closing,
            endingName);
        FirstLastTest($"<{tagFullName} b>", fullName, name, ns, TagEnding.ClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName} b=\"'>\" c='\"/>'>", fullName, name, ns, TagEnding.ClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName}\rb>", fullName, name, ns, TagEnding.ClosingHasAttributes,
            endingName2, endingName2);
        FirstLastTest($"<{tagFullName}\r\n\tb >", fullName, name, ns, TagEnding.ClosingHasAttributes,
            endingName2, attributeStart);

        FirstLastTest($"<{tagFullName}/>", fullName, name, ns, TagEnding.SelfClosing);
        FirstLastTest($"<{tagFullName} \r\n\t/>", fullName, name, ns, TagEnding.SelfClosing,
            endingName);
        FirstLastTest($"<{tagFullName} b />", fullName, name, ns, TagEnding.SelfClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName} b=\"'>\" c='\">'/>", fullName, name, ns, TagEnding.SelfClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName}\rb />", fullName, name, ns, TagEnding.SelfClosingHasAttributes,
            endingName2, endingName2);
        FirstLastTest($"<{tagFullName}\r\n\tb />", fullName, name, ns, TagEnding.SelfClosingHasAttributes,
            endingName2, attributeStart);

        FailFirstLastTest($"<{tagFullName}/", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} ", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} /", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b='", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b=''", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b='>", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b='>'", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b=\"", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b=\"\"", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b=\">", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b=\">\"", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b=\"/>", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} b=\"/>\"", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} '>", fullName, name, ns);
        FailFirstLastTest($"<{tagFullName} \">", fullName, name, ns);

        var encoding = _encoding;
        var closing = encoding.GetBytes($"<{tagFullName}>");
        var selfClosing = encoding.GetBytes($"<{tagFullName} />");

        var data = encoding.GetBytes($"<{tagFullName}><{tagFullName} /></{tagFullName}><{tagFullName} b=3 /><{tagFullName}\rc=4></{tagFullName}>");

        var tag = FirstTest(data, fullName, name, ns, TagEnding.Closing, TagEndings.AnyClosing);
        Assert.That(data[tag.Range].SequenceEqual(closing), Is.True);
        Assert.That(LastTest(data, fullName, name, ns, TagEnding.Closing, TagEndings.ClosingHasNoAttributes), Is.EqualTo(tag));

        tag = FirstTest(data, fullName, name, ns, TagEnding.SelfClosing, TagEndings.SelfClosing);
        Assert.That(data[tag.Range].SequenceEqual(selfClosing), Is.True);
        Assert.That(LastTest(data, fullName, name, ns, TagEnding.SelfClosing, TagEndings.SelfClosingHasNoAttributes), Is.EqualTo(tag));

        tag = FirstTest(data, fullName, name, ns, TagEnding.SelfClosingHasAttributes, TagEndings.SelfClosingHasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName} b=3 />")), Is.True);
        Assert.That(LastTest(data, fullName, name, ns, TagEnding.SelfClosingHasAttributes, TagEndings.SelfClosing), Is.EqualTo(tag));

        tag = FirstTest(data, fullName, name, ns, TagEnding.ClosingHasAttributes, TagEndings.ClosingHasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}\rc=4>")), Is.True);
        Assert.That(LastTest(data, fullName, name, ns, TagEnding.ClosingHasAttributes, TagEndings.AnyClosing), Is.EqualTo(tag));

        tag = FirstTest(data, fullName, name, ns, TagEnding.AttributeStart, TagEndings.HasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName} ")), Is.True);

        tag = LastTest(data, fullName, name, ns, TagEnding.AttributeStart, TagEndings.HasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagFullName}\r")), Is.True);
    }

    public void LastClosingTest(TagData tagData)
    {
        var encoding = _encoding;
        var finder = _finder;

        var hasNamespace = tagData.NameSpace.Length > 0;
        var tagNS = tagData.NameSpaceBytes;
        var name = tagData.NameBytes;
        var tagFullName = tagData.FullName;
        var fullName = tagData.FullNameBytes;

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
            closing = finder.LastClosing(data, fullName, out ns);
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

    private Tag FirstLastTest(string str, ReadOnlySpan<byte> fullName,
        ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var data = _encoding.GetBytes(str);

        var tag = FirstTest(data, fullName, name, ns, ending, endingName, attributeStart);

        Assert.That(LastTest(data, fullName, name, ns, ending, endingName, attributeStart), Is.EqualTo(tag));

        return tag;
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

        if (ending.HasNoAttributes())
        {
            Assert.That(tag.Ending, Is.EqualTo(ending));
        }
        else if (ending.HasAttributes())
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
            Assert.That(_finder.First(data, fullName, TagEndings.HasAttributes).IsEmpty, Is.True);
            Assert.That(_finder.First(data, name, ns, TagEndings.HasAttributes).IsEmpty, Is.True);
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

    private Tag LastTest(ReadOnlySpan<byte> data, ReadOnlySpan<byte> fullName,
        ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var availableEndings = GetAvailableEndings(ending);

        var endings = availableEndings[0];
        var tag = _finder.Last(data, fullName, endings);

        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        if (ending.HasNoAttributes())
        {
            Assert.That(tag.Ending, Is.EqualTo(ending));
        }
        else if (ending.HasAttributes())
        {
            Assert.That(tag.Ended, Is.EqualTo(ending));
        }
        else
        {
            Assert.Fail($"{ending} not support");
        }

        Assert.That(_finder.Last(data, name, ns, endings), Is.EqualTo(tag));

        for (int i = 1; i < availableEndings.Length; i++)
        {
            endings = availableEndings[i];
            Assert.That(_finder.Last(data, fullName, endings), Is.EqualTo(tag));
            Assert.That(_finder.Last(data, name, ns, endings), Is.EqualTo(tag));
        }

        foreach (var notAvailableEndings in GetNotAvailableEndings(ending))
        {
            Assert.That(_finder.Last(data, fullName, notAvailableEndings).IsEmpty, Is.True);
            Assert.That(_finder.Last(data, name, ns, notAvailableEndings).IsEmpty, Is.True);
        }

        if (endingName.IsEmpty)
        {
            Assert.That(_finder.Last(data, fullName, TagEndings.HasNoAttributes), Is.EqualTo(tag));
            Assert.That(_finder.Last(data, name, ns, TagEndings.HasNoAttributes), Is.EqualTo(tag));
        }
        else
        {
            var tag2 = _finder.Last(data, fullName, TagEndings.HasNoAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.Name));
            Assert.That(data[tag2.Range].SequenceEqual(endingName), Is.True);

            Assert.That(_finder.Last(data, name, ns, TagEndings.HasNoAttributes), Is.EqualTo(tag2));
        }

        //Без атрибутов
        if (attributeStart.IsEmpty)
        {
            Assert.That(_finder.Last(data, fullName, TagEndings.HasAttributes).IsEmpty, Is.True);
            Assert.That(_finder.Last(data, name, ns, TagEndings.HasAttributes).IsEmpty, Is.True);
        }
        else
        {
            var tag2 = _finder.Last(data, fullName, TagEndings.HasAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.AttributeStart));
            Assert.That(data[tag2.Range].SequenceEqual(attributeStart), Is.True);

            Assert.That(_finder.Last(data, name, ns, TagEndings.HasAttributes), Is.EqualTo(tag2));
        }

        return tag;
    }

    private void FailFirstLastTest(string str, ReadOnlySpan<byte> fullName,
        ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
    {
        var data = _encoding.GetBytes(str);
        Assert.That(_finder.First(data, fullName).IsEmpty, Is.True);
        Assert.That(_finder.First(data, name, ns).IsEmpty, Is.True);

        Assert.That(_finder.Last(data, fullName).IsEmpty, Is.True);
        Assert.That(_finder.Last(data, name, ns).IsEmpty, Is.True);
    }

    private Tag FirstTest(ReadOnlySpan<byte> data, ReadOnlySpan<byte> fullName,
        ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEnding ending, TagEndings endings)
    {
        var tag = _finder.First(data, fullName, endings);

        Assert.That(_finder.First(data, name, ns, endings), Is.EqualTo(tag));

        if (ending.HasNoAttributes())
        {
            Assert.That(tag.Ending, Is.EqualTo(ending));
        }
        else if (ending.HasAttributes())
        {
            Assert.That(tag.Ended, Is.EqualTo(ending));
        }
        else if (ending.IsNoClosing())
        {
            Assert.That(tag.Unended, Is.EqualTo(ending));
        }
        else
        {
            Assert.Fail($"{ending} not support");
        }

        return tag;
    }

    private Tag LastTest(ReadOnlySpan<byte> data, ReadOnlySpan<byte> fullName,
        ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEnding ending, TagEndings endings)
    {
        var tag = _finder.Last(data, fullName, endings);

        Assert.That(_finder.Last(data, name, ns, endings), Is.EqualTo(tag));

        if (ending.HasNoAttributes())
        {
            Assert.That(tag.Ending, Is.EqualTo(ending));
        }
        else if (ending.HasAttributes())
        {
            Assert.That(tag.Ended, Is.EqualTo(ending));
        }
        else if (ending.IsNoClosing())
        {
            Assert.That(tag.Unended, Is.EqualTo(ending));
        }
        else
        {
            Assert.Fail($"{ending} not support");
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