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
        FirstLastClosingTest(tagData);

        FirstLastTest(tagData);

        PairsTest(tagData);
    }

    public void PairsTest(TagData tagData)
    {
        var data = _encoding.GetBytes($"<{tagData}></{tagData}>").AsSpan();
        var first = _finder.FirstPair(data, tagData.Name, tagData.Namespace);
        Assert.That(data[first.Outer].SequenceEqual(data), Is.True);
        Assert.That(data[first.Inner].IsEmpty, Is.True);

        var last = _finder.LastPair(data, tagData.Name, out var ns);
        Assert.That(data[ns].SequenceEqual(tagData.Namespace), Is.True);
        Assert.That(data[last.Outer].SequenceEqual(data), Is.True);
        Assert.That(data[last.Inner].IsEmpty, Is.True);

        data = _encoding.GetBytes($"<{tagData}>first</{tagData}><{tagData}>last</{tagData}>").AsSpan();

        first = _finder.FirstPair(data, tagData.Name, tagData.Namespace);
        Assert.That(data[first.Outer].SequenceEqual(_encoding.GetBytes($"<{tagData}>first</{tagData}>")), Is.True);
        Assert.That(data[first.Inner].SequenceEqual(_encoding.GetBytes("first")), Is.True);

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

    #region FirstLastClosing

    public void FirstLastClosingTest(TagData tagData)
    {
        FirstLastClosing($"</{tagData}>", tagData);
        FirstLastClosing($"</{tagData} >", tagData, hasSpace: true);
        FirstLastClosing($"</{tagData}\n\r\t >", tagData, hasSpace: true);

        FailClosing($"</{tagData}\n\r\t b>", tagData);
        FailClosing($"</{tagData}\n\r\t ", tagData);
        FailClosing($"/{tagData}>", tagData);
        FailClosing($"<{tagData}>", tagData);
        FailClosing($"{tagData}>", tagData);
        FailClosing(tagData.FullName, tagData);

        if (!tagData.HasNamespace)
        {
            FailClosing($"<tag></tag><b c=\":{tagData}>\" />", tagData);
            FailClosing($"<tag></tag><b c=\"ns:{tagData}>\" />", tagData);
            FailClosing($"<tag></tag><b c=':{tagData} \r\n\t>' />", tagData);
            FailClosing($"<tag></tag><b c='ns:{tagData} \r\n\t>' />", tagData);
            FailClosing($"</ns/:{tagData}>", tagData);
        }
    }

    private TagClosing FirstLastClosing(string data, TagData tagData, bool hasSpace = false)
    {
        return FirstLastClosing(_encoding.GetBytes(data), tagData, hasSpace);
    }

    private TagClosing FirstLastClosing(ReadOnlySpan<byte> data, TagData tagData, bool hasSpace = false)
    {
        var tag = FirstClosing(data, tagData, hasSpace);

        Assert.That(LastClosing(data, tagData, hasSpace), Is.EqualTo(tag));

        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        return tag;
    }

    #region Success

    private TagClosing FirstClosing(string data, TagData tagData, bool hasSpace = false)
    {
        return FirstClosing(_encoding.GetBytes(data), tagData, hasSpace);
    }

    private TagClosing FirstClosing(ReadOnlySpan<byte> data, TagData tagData, bool hasSpace = false)
    {
        var tag = _finder.FirstClosing(data, tagData.FullName);
        Assert.That(_finder.FirstClosing(data, tagData.Name, tagData.Namespace), Is.EqualTo(tag));

        Assert.That(tag.HasSpace, Is.EqualTo(hasSpace));
        /*
        Assert.That(_finder.FirstClosing(data, tagData.Name, out var ns), Is.EqualTo(tag));
        Assert.That(data[ns].SequenceEqual(tagData.Namespace), Is.True);

        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastClosing(data, tagData.FullName, out ns), Is.EqualTo(tag));
            Assert.That(data[ns].IsEmpty, Is.True);
        }*/

        return tag;
    }

    private TagClosing LastClosing(string data, TagData tagData, bool hasSpace = false)
    {
        return LastClosing(_encoding.GetBytes(data), tagData, hasSpace);
    }

    private TagClosing LastClosing(ReadOnlySpan<byte> data, TagData tagData, bool hasSpace = false)
    {
        var tag = _finder.LastClosing(data, tagData.FullName);
        Assert.That(_finder.LastClosing(data, tagData.Name, tagData.Namespace), Is.EqualTo(tag));
        Assert.That(_finder.LastClosing(data, tagData.Name, out var ns), Is.EqualTo(tag));
        Assert.That(data[ns].SequenceEqual(tagData.Namespace), Is.True);

        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastClosing(data, tagData.FullName, out ns), Is.EqualTo(tag));
            Assert.That(data[ns].IsEmpty, Is.True);
        }

        Assert.That(tag.HasSpace, Is.EqualTo(hasSpace));

        return tag;
    }

    #endregion Success

    #region Fail

    private void FailClosing(string str, TagData tagData)
    {
        FailClosing(_encoding.GetBytes(str), tagData);
    }

    private void FailClosing(ReadOnlySpan<byte> data, TagData tagData)
    {
        FailFirstClosing(data, tagData);
        FailLastClosing(data, tagData);
    }

    private void FailFirstClosing(ReadOnlySpan<byte> data, TagData tagData)
    {
        var tag = _finder.FirstClosing(data, tagData.FullName);
        Assert.That(_finder.FirstClosing(data, tagData.Name, tagData.Namespace), Is.EqualTo(tag));
        /*
        Assert.That(_finder.FirstClosing(data, tagData.Name, out var ns), Is.EqualTo(tag));
        Assert.That(ns.Start.Value == 0 && ns.End.Value == 0, Is.True);

        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastClosing(data, tagData.FullName, out ns), Is.EqualTo(tag));
            Assert.That(data[ns].IsEmpty, Is.True);
        }*/

        Assert.That(tag.IsEmpty, Is.True);
    }

    private void FailLastClosing(ReadOnlySpan<byte> data, TagData tagData)
    {
        var tag = _finder.LastClosing(data, tagData.FullName);
        Assert.That(_finder.LastClosing(data, tagData.Name, tagData.Namespace), Is.EqualTo(tag));
        Assert.That(_finder.LastClosing(data, tagData.Name, out var ns), Is.EqualTo(tag));
        Assert.That(ns.Start.Value == 0 && ns.End.Value == 0, Is.True);

        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastClosing(data, tagData.FullName, out ns), Is.EqualTo(tag));
            Assert.That(data[ns].IsEmpty, Is.True);
        }

        Assert.That(tag.IsEmpty, Is.True);
    }

    #endregion Fail

    #endregion LastClosing

    #region FirstLast

    public void FirstLastTest(TagData tagData)
    {
        var endingName = _encoding.GetBytes($"<{tagData} ");
        var endingName2 = _encoding.GetBytes($"<{tagData}\r");
        var attributeStart = _encoding.GetBytes($"<{tagData}\r\n\t");

        FirstLast($"<{tagData}>", tagData, TagEnding.Closing);
        FirstLast($"<{tagData} \r\n\t>", tagData, TagEnding.Closing, endingName);
        FirstLast($"<{tagData} b>", tagData, TagEnding.ClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData} b=\"'>\" c='\"/>'>", tagData, TagEnding.ClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData}\rb>", tagData, TagEnding.ClosingHasAttributes, endingName2, endingName2);
        FirstLast($"<{tagData}\r\n\tb >", tagData, TagEnding.ClosingHasAttributes, endingName2, attributeStart);

        FirstLast($"<{tagData}/>", tagData, TagEnding.SelfClosing);
        FirstLast($"<{tagData} \r\n\t/>", tagData, TagEnding.SelfClosing, endingName);
        FirstLast($"<{tagData} b />", tagData, TagEnding.SelfClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData} b=\"'>\" c='\">'/>", tagData, TagEnding.SelfClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData}\rb />", tagData, TagEnding.SelfClosingHasAttributes, endingName2, endingName2);
        FirstLast($"<{tagData}\r\n\tb />", tagData, TagEnding.SelfClosingHasAttributes, endingName2, attributeStart);

        FailFirstLast($"<{tagData}/", tagData);
        FailFirstLast($"<{tagData} ", tagData);
        FailFirstLast($"<{tagData} /", tagData);
        FailFirstLast($"<{tagData} b='", tagData);
        FailFirstLast($"<{tagData} b=''", tagData);
        FailFirstLast($"<{tagData} b='>", tagData);
        FailFirstLast($"<{tagData} b='>'", tagData);
        FailFirstLast($"<{tagData} b=\"", tagData);
        FailFirstLast($"<{tagData} b=\"\"", tagData);
        FailFirstLast($"<{tagData} b=\">", tagData);
        FailFirstLast($"<{tagData} b=\">\"", tagData);
        FailFirstLast($"<{tagData} b=\"/>", tagData);
        FailFirstLast($"<{tagData} b=\"/>\"", tagData);
        FailFirstLast($"<{tagData} b=\"\"/ >", tagData);
        FailFirstLast($"<{tagData} '>", tagData);
        FailFirstLast($"<{tagData} \">", tagData);

        FailFirstLast($"<{tagData} /", tagData, TagEndings.HasAttributes);

        var encoding = _encoding;
        var closing = encoding.GetBytes($"<{tagData}>");
        var selfClosing = encoding.GetBytes($"<{tagData} />");

        var data = encoding.GetBytes($"<{tagData}><{tagData} /></{tagData}><{tagData} b=3 /><{tagData}\rc=4></{tagData}>");

        var tag = First(data, tagData, TagEnding.Closing, TagEndings.AnyClosing);
        Assert.That(data[tag.Range].SequenceEqual(closing), Is.True);
        Assert.That(Last(data, tagData, TagEnding.Closing, TagEndings.ClosingHasNoAttributes), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.SelfClosing, TagEndings.SelfClosing);
        Assert.That(data[tag.Range].SequenceEqual(selfClosing), Is.True);
        Assert.That(Last(data, tagData, TagEnding.SelfClosing, TagEndings.SelfClosingHasNoAttributes), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.SelfClosingHasAttributes, TagEndings.SelfClosingHasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagData} b=3 />")), Is.True);
        Assert.That(Last(data, tagData, TagEnding.SelfClosingHasAttributes, TagEndings.SelfClosing), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.ClosingHasAttributes, TagEndings.ClosingHasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagData}\rc=4>")), Is.True);
        Assert.That(Last(data, tagData, TagEnding.ClosingHasAttributes, TagEndings.AnyClosing), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.AttributeStart, TagEndings.HasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagData} ")), Is.True);

        tag = Last(data, tagData, TagEnding.AttributeStart, TagEndings.HasAttributes);
        Assert.That(data[tag.Range].SequenceEqual(encoding.GetBytes($"<{tagData}\r")), Is.True);
    }

    private Tag FirstLast(string str, TagData tagData, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var data = _encoding.GetBytes(str);

        var tag = First(data, tagData, ending, endingName, attributeStart);

        Assert.That(Last(data, tagData, ending, endingName, attributeStart), Is.EqualTo(tag));

        return tag;
    }

    private Tag First(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var availableEndings = GetAvailableEndings(ending);

        var endings = availableEndings[0];
        var tag = _finder.First(data, tagData.FullName, endings);

        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        EndingTest(tag, ending);

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

    private Tag Last(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending,
        ReadOnlySpan<byte> endingName = default,
        ReadOnlySpan<byte> attributeStart = default)
    {
        var availableEndings = GetAvailableEndings(ending);

        var endings = availableEndings[0];
        var tag = _finder.Last(data, tagData.FullName, endings);

        Assert.That(data[tag.Range].SequenceEqual(data), Is.True);

        EndingTest(tag, ending);

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

    private void FailFirstLast(string str, TagData tagData, TagEndings endings = default)
    {
        var data = _encoding.GetBytes(str);
        Assert.That(_finder.First(data, tagData.FullName, endings).IsEmpty, Is.True);
        Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings).IsEmpty, Is.True);

        Assert.That(_finder.Last(data, tagData.FullName, endings).IsEmpty, Is.True);
        Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, endings).IsEmpty, Is.True);
    }

    private Tag First(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending, TagEndings endings)
    {
        var tag = _finder.First(data, tagData.FullName, endings);

        Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));

        EndingTest(tag, ending);

        return tag;
    }

    private Tag Last(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending, TagEndings endings)
    {
        var tag = _finder.Last(data, tagData.FullName, endings);

        Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));

        EndingTest(tag, ending);

        return tag;
    }

    #endregion FirstLast

    #region Static

    private static void EndingTest(Tag tag, TagEnding ending)
    {
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

    #endregion Static
}