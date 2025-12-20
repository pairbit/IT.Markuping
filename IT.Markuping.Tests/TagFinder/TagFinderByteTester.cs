using IT.Markuping.Extensions;
using IT.Markuping.Implementation;
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
        private byte[] NameBytes { get; }

        private byte[] NamespaceBytes { get; }

        private byte[] FullNameBytes { get; }

        public string NameString { get; }

        public string NamespaceString { get; }

        public string FullNameString { get; }

        public ReadOnlySpan<byte> Name => NameBytes;

        public ReadOnlySpan<byte> Namespace => NamespaceBytes;

        public ReadOnlySpan<byte> FullName => FullNameBytes;

        public bool HasNamespace => NamespaceString.Length > 0;

        public TagData(Encoding encoding, string name)
        {
            FullNameString = NameString = name;
            FullNameBytes = NameBytes = encoding.GetBytes(name);
            NamespaceString = string.Empty;
            NamespaceBytes = [];
        }

        public TagData(Encoding encoding, string name, string nameSpace)
        {
            NameString = name;
            NamespaceString = nameSpace;
            FullNameString = nameSpace + ":" + name;
            NameBytes = encoding.GetBytes(name);
            NamespaceBytes = encoding.GetBytes(nameSpace);
            FullNameBytes = encoding.GetBytes(FullNameString);
        }

        public override string ToString() => FullNameString;
    }

    public void Test()
    {
        InternalTest();
        Test(new(_encoding, "a"));
        Test(new(_encoding, "a", "n"));
    }

    private void InternalTest()
    {
        var startOpeningNS = _encoding.GetBytes("<ns:");
        var startClosing = _encoding.GetBytes("</");
        var startClosingNS = _encoding.GetBytes("</ns:");
        var ns = _encoding.GetBytes("ns");
        if (_finder is ByteTagFinder byteFinder)
        {
            Assert.That(byteFinder.IsStartClosing(startClosing, 0), Is.True);
            Assert.That(byteFinder.IsStartClosing(startClosingNS, 0, ns), Is.True);
            Assert.That(byteFinder.IsStartOpening(startOpeningNS, 0, ns), Is.True);
        }
        else if (_finder is BytesTagFinder bytesFinder)
        {
            Assert.That(bytesFinder.IsStartClosing(startClosing, 0), Is.True);
            Assert.That(bytesFinder.IsStartClosing(startClosingNS, 0, ns), Is.True);
            Assert.That(bytesFinder.IsStartOpening(startOpeningNS, 0, ns), Is.True);
        }
    }

    public void Test(TagData tagData)
    {
        LastClosingTest(tagData);

        FirstLastTest(tagData);

        Pairs(tagData);
    }

    public void Pairs(TagData tagData)
    {
        var data = _encoding.GetBytes($"<{tagData}></{tagData}>").AsSpan();
        var last = _finder.LastPair(data, tagData.Name, out var ns);
        Assert.That(data[ns].SequenceEqual(tagData.Namespace), Is.True);
        Assert.That(data[last.Outer].SequenceEqual(data), Is.True);
        Assert.That(data[last.Inner].IsEmpty, Is.True);

        data = _encoding.GetBytes($"<{tagData}>first</{tagData}><{tagData}>last</{tagData}>").AsSpan();
        last = _finder.LastPair(data, tagData.Name, out ns);
        Assert.That(data[ns].SequenceEqual(tagData.Namespace), Is.True);
        Assert.That(data[last.Outer].SequenceEqual(_encoding.GetBytes($"<{tagData}>last</{tagData}>")), Is.True);
        Assert.That(data[last.Inner].SequenceEqual(_encoding.GetBytes("last")), Is.True);

        //data = _encoding.GetBytes($"<{tagFullName}><{tagFullName}>1</{tagFullName}></{tagFullName}>").AsSpan();
        //last = _finder.LastPair(data, name, out ns);
        //Assert.That(last.HasNamespace, Is.EqualTo(hasNamespace));
        //Assert.That(data[ns].SequenceEqual(tag.NameSpaceBytes), Is.True);
        //Assert.That(data[last.Outer].SequenceEqual(data), Is.True);
        //Assert.That(data[last.Inner].SequenceEqual(_encoding.GetBytes($"<{tagFullName}>1</{tagFullName}>")), Is.True);
    }

    public void FirstLastTest(TagData tagData)
    {
        var tagFullName = tagData.FullNameString;
        var fullName = tagData.FullName;
        var name = tagData.Name;
        var ns = tagData.Namespace;

        var endingName = _encoding.GetBytes($"<{tagFullName} ");
        var endingName2 = _encoding.GetBytes($"<{tagFullName}\r");
        var attributeStart = _encoding.GetBytes($"<{tagFullName}\r\n\t");

        FirstLastTest($"<{tagFullName}>", tagData, TagEnding.Closing);
        FirstLastTest($"<{tagFullName} \r\n\t>", tagData, TagEnding.Closing,
            endingName);
        FirstLastTest($"<{tagFullName} b>", tagData, TagEnding.ClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName} b=\"'>\" c='\"/>'>", tagData, TagEnding.ClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName}\rb>", tagData, TagEnding.ClosingHasAttributes,
            endingName2, endingName2);
        FirstLastTest($"<{tagFullName}\r\n\tb >", tagData, TagEnding.ClosingHasAttributes,
            endingName2, attributeStart);

        FirstLastTest($"<{tagFullName}/>", tagData, TagEnding.SelfClosing);
        FirstLastTest($"<{tagFullName} \r\n\t/>", tagData, TagEnding.SelfClosing,
            endingName);
        FirstLastTest($"<{tagFullName} b />", tagData, TagEnding.SelfClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName} b=\"'>\" c='\">'/>", tagData, TagEnding.SelfClosingHasAttributes,
            endingName, endingName);
        FirstLastTest($"<{tagFullName}\rb />", tagData, TagEnding.SelfClosingHasAttributes,
            endingName2, endingName2);
        FirstLastTest($"<{tagFullName}\r\n\tb />", tagData, TagEnding.SelfClosingHasAttributes,
            endingName2, attributeStart);

        FailFirstLastTest($"<{tagFullName}/", tagData);
        FailFirstLastTest($"<{tagFullName} ", tagData);
        FailFirstLastTest($"<{tagFullName} /", tagData);
        FailFirstLastTest($"<{tagFullName} b='", tagData);
        FailFirstLastTest($"<{tagFullName} b=''", tagData);
        FailFirstLastTest($"<{tagFullName} b='>", tagData);
        FailFirstLastTest($"<{tagFullName} b='>'", tagData);
        FailFirstLastTest($"<{tagFullName} b=\"", tagData);
        FailFirstLastTest($"<{tagFullName} b=\"\"", tagData);
        FailFirstLastTest($"<{tagFullName} b=\">", tagData);
        FailFirstLastTest($"<{tagFullName} b=\">\"", tagData);
        FailFirstLastTest($"<{tagFullName} b=\"/>", tagData);
        FailFirstLastTest($"<{tagFullName} b=\"/>\"", tagData);
        FailFirstLastTest($"<{tagFullName} b=\"\"/ >", tagData);
        FailFirstLastTest($"<{tagFullName} '>", tagData);
        FailFirstLastTest($"<{tagFullName} \">", tagData);

        FailFirstLastTest($"<{tagFullName} /", tagData, TagEndings.HasAttributes);

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

    #region LastClosing

    public void LastClosingTest(TagData tagData)
    {
        LastClosing($"</{tagData}>", tagData);
        LastClosing($"</{tagData} >", tagData, hasSpace: true);
        LastClosing($"</{tagData}\n\r\t >", tagData, hasSpace: true);

        //Fail
        LastClosing($"</{tagData}\n\r\t b>", tagData, fail: true);
        LastClosing($"</{tagData}\n\r\t ", tagData, fail: true);
        LastClosing($"/{tagData}>", tagData, fail: true);
        LastClosing($"<{tagData}>", tagData, fail: true);
        LastClosing($"{tagData}>", tagData, fail: true);
        LastClosing(tagData.FullName, tagData, fail: true);

        if (!tagData.HasNamespace)
        {
            var data = _encoding.GetBytes($"<tag></tag><b c=\":{tagData}>\" />");
            var closing = _finder.LastClosing(data, tagData.Name, out var ns);
            Assert.That(closing.IsEmpty, Is.True);
            Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

            data = _encoding.GetBytes($"<tag></tag><b c=\"ns:{tagData}>\" />");
            closing = _finder.LastClosing(data, tagData.Name, out ns);
            Assert.That(closing.IsEmpty, Is.True);
            Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

            data = _encoding.GetBytes($"<tag></tag><b c=':{tagData} \r\n\t>' />");
            closing = _finder.LastClosing(data, tagData.Name, out ns);
            Assert.That(closing.IsEmpty, Is.True);
            Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

            data = _encoding.GetBytes($"<tag></tag><b c='ns:{tagData} \r\n\t>' />");
            closing = _finder.LastClosing(data, tagData.Name, out ns);
            Assert.That(closing.IsEmpty, Is.True);
            Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));

            data = _encoding.GetBytes($"</ns/:{tagData}>");
            closing = _finder.LastClosing(data, tagData.Name, out ns);
            Assert.That(closing.IsEmpty, Is.True);
            Assert.That(ns.Start.Value, Is.EqualTo(ns.End.Value));
        }
    }

    private TagClosing LastClosing(string data, TagData tagData, bool hasSpace = false, bool fail = false)
    {
        return LastClosing(_encoding.GetBytes(data), tagData, hasSpace, fail);
    }

    private TagClosing LastClosing(ReadOnlySpan<byte> data, TagData tagData, bool hasSpace = false, bool fail = false)
    {
        var tag = _finder.LastClosing(data, tagData.FullName);

        Assert.That(_finder.LastClosing(data, tagData.Name, tagData.Namespace), Is.EqualTo(tag));
        Assert.That(_finder.LastClosing(data, tagData.Name, out var ns), Is.EqualTo(tag));
        if (fail)
        {
            Assert.That(ns.Start.Value == 0 && ns.End.Value == 0, Is.True);
        }
        else
        {
            Assert.That(data[ns].SequenceEqual(tagData.Namespace), Is.True);
        }

        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastClosing(data, tagData.FullName, out ns), Is.EqualTo(tag));
            Assert.That(data[ns].IsEmpty, Is.True);
        }

        if (fail)
        {
            Assert.That(tag.IsEmpty, Is.True);
        }
        else
        {
            Assert.That(data[tag.Range].SequenceEqual(data), Is.True);
            Assert.That(tag.HasSpace, Is.EqualTo(hasSpace));
        }

        return tag;
    }

    #endregion LastClosing

    private Tag FirstLastTest(string str, TagData tagData, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var data = _encoding.GetBytes(str);

        var tag = FirstTest(data, tagData, ending, endingName, attributeStart);

        Assert.That(LastTest(data, tagData, ending, endingName, attributeStart), Is.EqualTo(tag));

        return tag;
    }

    private Tag FirstTest(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var availableEndings = GetAvailableEndings(ending);

        var endings = availableEndings[0];
        var tag = _finder.First(data, tagData.FullName, endings);

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

        Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));

        for (int i = 1; i < availableEndings.Length; i++)
        {
            endings = availableEndings[i];
            Assert.That(_finder.First(data, tagData.FullName, endings), Is.EqualTo(tag));
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));
        }

        foreach (var notAvailableEndings in GetNotAvailableEndings(ending))
        {
            Assert.That(_finder.First(data, tagData.FullName, notAvailableEndings).IsEmpty, Is.True);
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, notAvailableEndings).IsEmpty, Is.True);
        }

        if (endingName.IsEmpty)
        {
            Assert.That(_finder.First(data, tagData.FullName, TagEndings.HasNoAttributes), Is.EqualTo(tag));
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasNoAttributes), Is.EqualTo(tag));
        }
        else
        {
            var tag2 = _finder.First(data, tagData.FullName, TagEndings.HasNoAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.Name));
            Assert.That(data[tag2.Range].SequenceEqual(endingName), Is.True);

            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasNoAttributes), Is.EqualTo(tag2));
        }

        //Без атрибутов
        if (attributeStart.IsEmpty)
        {
            Assert.That(_finder.First(data, tagData.FullName, TagEndings.HasAttributes).IsEmpty, Is.True);
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasAttributes).IsEmpty, Is.True);
        }
        else
        {
            var tag2 = _finder.First(data, tagData.FullName, TagEndings.HasAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.AttributeStart));
            Assert.That(data[tag2.Range].SequenceEqual(attributeStart), Is.True);

            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasAttributes), Is.EqualTo(tag2));
        }

        return tag;
    }

    private Tag LastTest(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var availableEndings = GetAvailableEndings(ending);

        var endings = availableEndings[0];
        var tag = _finder.Last(data, tagData.FullName, endings);

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

        Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));

        for (int i = 1; i < availableEndings.Length; i++)
        {
            endings = availableEndings[i];
            Assert.That(_finder.Last(data, tagData.FullName, endings), Is.EqualTo(tag));
            Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));
        }

        foreach (var notAvailableEndings in GetNotAvailableEndings(ending))
        {
            Assert.That(_finder.Last(data, tagData.FullName, notAvailableEndings).IsEmpty, Is.True);
            Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, notAvailableEndings).IsEmpty, Is.True);
        }

        if (endingName.IsEmpty)
        {
            Assert.That(_finder.Last(data, tagData.FullName, TagEndings.HasNoAttributes), Is.EqualTo(tag));
            Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, TagEndings.HasNoAttributes), Is.EqualTo(tag));
        }
        else
        {
            var tag2 = _finder.Last(data, tagData.FullName, TagEndings.HasNoAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.Name));
            Assert.That(data[tag2.Range].SequenceEqual(endingName), Is.True);

            Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, TagEndings.HasNoAttributes), Is.EqualTo(tag2));
        }

        //Без атрибутов
        if (attributeStart.IsEmpty)
        {
            Assert.That(_finder.Last(data, tagData.FullName, TagEndings.HasAttributes).IsEmpty, Is.True);
            Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, TagEndings.HasAttributes).IsEmpty, Is.True);
        }
        else
        {
            var tag2 = _finder.Last(data, tagData.FullName, TagEndings.HasAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.AttributeStart));
            Assert.That(data[tag2.Range].SequenceEqual(attributeStart), Is.True);

            Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, TagEndings.HasAttributes), Is.EqualTo(tag2));
        }

        return tag;
    }

    private void FailFirstLastTest(string str, TagData tagData, TagEndings endings = default)
    {
        var data = _encoding.GetBytes(str);
        Assert.That(_finder.First(data, tagData.FullName, endings).IsEmpty, Is.True);
        Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings).IsEmpty, Is.True);

        Assert.That(_finder.Last(data, tagData.FullName, endings).IsEmpty, Is.True);
        Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, endings).IsEmpty, Is.True);
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
        TagEndings.AnyClosing, TagEndings.AnyClosingHasAttributes, (TagEndings)15,
        TagEndings.Closing, TagEndings.ClosingHasAttributes];

    private static readonly TagEndings[] SelfClosing_Available_Endings = [
        TagEndings.AnyClosing, TagEndings.AnyClosingHasNoAttributes, (TagEndings)15,
        TagEndings.SelfClosing, TagEndings.SelfClosingHasNoAttributes];

    private static readonly TagEndings[] SelfClosingHasAttributes_Available_Endings = [
        TagEndings.AnyClosing, TagEndings.AnyClosingHasAttributes, (TagEndings)15,
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