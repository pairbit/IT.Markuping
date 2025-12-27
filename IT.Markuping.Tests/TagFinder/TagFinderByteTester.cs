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
        InternalTest();
        Test(new(_encoding, "a"));
        Test(new(_encoding, "a", "n"));

        //TODO: BUG #8
        var data = _encoding.GetBytes("</ns:a>").AsSpan();
        var tag = _finder.First(data, _encoding.GetBytes("a"), out var ns);
        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(data), Is.True);
        Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(_encoding.GetBytes("/ns")), Is.True);
    }

    private void InternalTest()
    {
        var startOpeningNS = _encoding.GetBytes("<ns:");
        var startClosing = _encoding.GetBytes("</");
        var startClosingNS = _encoding.GetBytes("</ns:");
        var ns = _encoding.GetBytes("ns");
        //if (_finder is TagFinder<byte> tagFinder)
        //{
        //    Assert.That(tagFinder.IsStartClosing(startClosing, 0), Is.True);
        //    Assert.That(tagFinder.IsStartClosing(startClosingNS, 0, ns), Is.True);
        //    Assert.That(tagFinder.IsStartOpening(startOpeningNS, 0, ns), Is.True);
        //}
    }

    public void Test(TagData tagData)
    {
        FirstLastClosingTest(tagData);

        FirstLastTest(tagData);

        PairsTest(tagData);
    }

    #region PairsTest

    public void PairsTest(TagData tagData)
    {
        FirstLastPair($"<{tagData}></{tagData}>", tagData, string.Empty);
        FirstLastPair($"<{tagData}>inner</{tagData}>", tagData, "inner");
        FirstLastPair($"<{tagData} ab>inner</{tagData} >", tagData, "inner");
        FirstLastPair($"<{tagData} c=\"'>'\"><tag></tag></{tagData} >", tagData, "<tag></tag>");
        FirstLastPair($"<{tagData} b c='\">\"'><{tagData}/></{tagData} \t\r\n>", tagData, $"<{tagData}/>");

        FirstLastPair($"{tagData}<{tagData}></{tagData}>{tagData}", tagData, string.Empty,
            $"<{tagData}></{tagData}>");

        var data = _encoding.GetBytes($"<{tagData}>first</{tagData}><{tagData}>last</{tagData}>").AsSpan();

        var first = FirstPair(data, tagData);
        Assert.That(data.Slice(first.Start, first.Length).SequenceEqual(_encoding.GetBytes($"<{tagData}>first</{tagData}>")), Is.True);
        Assert.That(data.Slice(first.InnerStart, first.InnerLength).SequenceEqual(_encoding.GetBytes("first")), Is.True);

        var last = LastPair(data, tagData);
        Assert.That(data.Slice(last.Start, last.Length).SequenceEqual(_encoding.GetBytes($"<{tagData}>last</{tagData}>")), Is.True);
        Assert.That(data.Slice(last.InnerStart, last.InnerLength).SequenceEqual(_encoding.GetBytes("last")), Is.True);

        Assert.That(first, Is.Not.EqualTo(last));

        FirstLastPair($"<{tagData}><{tagData}>1</{tagData}></{tagData}>", tagData,
            $"<{tagData}>1</{tagData}>", nodesCount: 1);

        FirstLastPair($"<{tagData}><{tagData}>1</{tagData}><{tagData}>2</{tagData}></{tagData}>", tagData,
            $"<{tagData}>1</{tagData}><{tagData}>2</{tagData}>", nodesCount: 2);

        FirstLastPair($"<{tagData}><{tagData}><{tagData}>2</{tagData}></{tagData}></{tagData}>", tagData,
            $"<{tagData}><{tagData}>2</{tagData}></{tagData}>", nodesCount: 2);

        FirstLastPair($"<{tagData}><{tagData}><{tagData}>1</{tagData}><{tagData}>2</{tagData}></{tagData}><{tagData}><{tagData}>3</{tagData}><{tagData}>4</{tagData}></{tagData}></{tagData}>", tagData,
            $"<{tagData}><{tagData}>1</{tagData}><{tagData}>2</{tagData}></{tagData}><{tagData}><{tagData}>3</{tagData}><{tagData}>4</{tagData}></{tagData}>", nodesCount: 6);
    }

    private void FirstLastPair(string data, TagData tagData, string inner, string? outer = null, int nodesCount = 0)
    {
        FirstLastPair(_encoding.GetBytes(data), tagData, _encoding.GetBytes(inner),
            outer == null ? [] : _encoding.GetBytes(outer), nodesCount);
    }

    private void FirstLastPair(ReadOnlySpan<byte> data, TagData tagData, ReadOnlySpan<byte> inner,
        ReadOnlySpan<byte> outer = default, int nodesCount = 0)
    {
        var tags = FirstPair(data, tagData, nodesCount);
        Assert.That(LastPair(data, tagData, nodesCount), Is.EqualTo(tags));

        if (outer.IsEmpty) outer = data;
        Assert.That(data.Slice(tags.Start, tags.Length).SequenceEqual(outer), Is.True);
        Assert.That(data.Slice(tags.InnerStart, tags.InnerLength).SequenceEqual(inner), Is.True);
    }

    private Tags FirstPair(ReadOnlySpan<byte> data, TagData tagData, int nodesCount = 0)
    {
        var tags = _finder.FirstPair(data, tagData.FullName, out var nodes);
        Assert.That(nodes, Is.EqualTo(nodesCount));
        Assert.That(_finder.FirstPair(data, tagData.Name, out nodes, tagData.Namespace), Is.EqualTo(tags));
        Assert.That(nodes, Is.EqualTo(nodesCount));
        Assert.That(_finder.FirstPair(data, tagData.Name, out nodes, out var ns), Is.EqualTo(tags));
        Assert.That(nodes, Is.EqualTo(nodesCount));
        Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
        if (tagData.HasNamespace)
        {
            Assert.That(_finder.FirstPair(data, tagData.FullName, out nodes, out ns), Is.EqualTo(tags));
            Assert.That(ns.IsEmpty, Is.True);
            Assert.That(nodes, Is.EqualTo(nodesCount));
        }
        Assert.That(tags.IsTree, Is.EqualTo(nodesCount > 0));
        return tags;
    }

    private Tags LastPair(ReadOnlySpan<byte> data, TagData tagData, int nodesCount = 0)
    {
        var tags = _finder.LastPair(data, tagData.FullName, out var nodes);
        Assert.That(nodes, Is.EqualTo(nodesCount));
        Assert.That(_finder.LastPair(data, tagData.Name, out nodes, tagData.Namespace), Is.EqualTo(tags));
        Assert.That(nodes, Is.EqualTo(nodesCount));
        Assert.That(_finder.LastPair(data, tagData.Name, out nodes, out var ns), Is.EqualTo(tags));
        Assert.That(nodes, Is.EqualTo(nodesCount));
        Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastPair(data, tagData.FullName, out nodes, out ns), Is.EqualTo(tags));
            Assert.That(ns.IsEmpty, Is.True);
            Assert.That(nodes, Is.EqualTo(nodesCount));
        }
        Assert.That(tags.IsTree, Is.EqualTo(nodesCount > 0));
        return tags;
    }

    #endregion PairsTest

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
        FailClosing($"</{tagData}", tagData);
        FailClosing(tagData.FullName, tagData);

        if (!tagData.HasNamespace)
        {
            //TODO: =
            //FailClosing($"<tag></tag><b c=:{tagData}> />", tagData);
            FailClosing($"<tag></tag><b c=\":{tagData}>\" />", tagData);
            FailClosing($"<tag></tag><b c=\"ns:{tagData}>\" />", tagData);
            FailClosing($"<tag></tag><b c=':{tagData} \r\n\t>' />", tagData);
            FailClosing($"<tag></tag><b c='ns:{tagData} \r\n\t>' />", tagData);
            FailClosing($"</ns/:{tagData}>", tagData);
            FailClosing($"</:{tagData}>", tagData);
        }
    }

    private void FirstLastClosing(string data, TagData tagData, bool hasSpace = false)
    {
        FirstLastClosing(_encoding.GetBytes(data), tagData, hasSpace);
    }

    private void FirstLastClosing(ReadOnlySpan<byte> data, TagData tagData, bool hasSpace = false)
    {
        EqFirstLastClosing(data, tagData, hasSpace);

        //Double data
        Span<byte> dd = new byte[data.Length * 2];
        data.CopyTo(dd);
        data.CopyTo(dd.Slice(data.Length));

        //var ddstr = _encoding.GetString(dd);

        NeqFirstLastClosing(dd, data, tagData, hasSpace);
    }

    private void EqFirstLastClosing(ReadOnlySpan<byte> data, TagData tagData, bool hasSpace = false)
    {
        var tag = FirstClosing(data, tagData, hasSpace);
        Assert.That(LastClosing(data, tagData, hasSpace), Is.EqualTo(tag));

        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(data), Is.True);
    }

    private void NeqFirstLastClosing(ReadOnlySpan<byte> dd, ReadOnlySpan<byte> data, TagData tagData, bool hasSpace = false)
    {
        var first = FirstClosing(dd, tagData, hasSpace);
        Assert.That(dd.Slice(first.Start, first.Length).SequenceEqual(data), Is.True);

        var last = LastClosing(dd, tagData, hasSpace);
        Assert.That(dd.Slice(last.Start, last.Length).SequenceEqual(data), Is.True);

        Assert.That(first, Is.Not.EqualTo(last));
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
        Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);

        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastClosing(data, tagData.FullName, out ns), Is.EqualTo(tag));
            Assert.That(ns.IsEmpty, Is.True);
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

        Assert.That(tag.IsEmpty, Is.True);
    }

    private void FailLastClosing(ReadOnlySpan<byte> data, TagData tagData)
    {
        var tag = _finder.LastClosing(data, tagData.FullName);
        Assert.That(_finder.LastClosing(data, tagData.Name, tagData.Namespace), Is.EqualTo(tag));
        Assert.That(_finder.LastClosing(data, tagData.Name, out var ns), Is.EqualTo(tag));
        Assert.That(ns.IsEmpty, Is.True);

        if (tagData.HasNamespace)
        {
            Assert.That(_finder.LastClosing(data, tagData.FullName, out ns), Is.EqualTo(tag));
            Assert.That(ns.IsEmpty, Is.True);
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
        FirstLast($"<{tagData} {tagData}>", tagData, TagEnding.ClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData} {tagData}='{tagData}>'>", tagData, TagEnding.ClosingHasAttributes, endingName, endingName);

        FirstLast($"<{tagData}/>", tagData, TagEnding.SelfClosing);
        FirstLast($"<{tagData} \r\n\t/>", tagData, TagEnding.SelfClosing, endingName);
        FirstLast($"<{tagData} b />", tagData, TagEnding.SelfClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData} b=\"'>\" c='\">'/>", tagData, TagEnding.SelfClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData}\rb />", tagData, TagEnding.SelfClosingHasAttributes, endingName2, endingName2);
        FirstLast($"<{tagData}\r\n\tb />", tagData, TagEnding.SelfClosingHasAttributes, endingName2, attributeStart);
        FirstLast($"<{tagData} {tagData}/>", tagData, TagEnding.SelfClosingHasAttributes, endingName, endingName);
        FirstLast($"<{tagData} {tagData}='{tagData}>'/>", tagData, TagEnding.SelfClosingHasAttributes, endingName, endingName);

        FailFirstLast(tagData.FullName, tagData);
        FailFirstLast($"<{tagData}", tagData);
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

        if (!tagData.HasNamespace)
        {
            //TODO: =
            //FailFirstLast($"<b c=:{tagData}> />", tagData);
            FailFirstLast($"<b c=\":{tagData}>\" />", tagData);
            FailFirstLast($"<b c=\"ns:{tagData}>\" />", tagData);
            FailFirstLast($"<b c=':{tagData} \r\n\t>' />", tagData);
            FailFirstLast($"<b c='ns:{tagData} \r\n\t>' />", tagData);
            FailFirstLast($"<b c=\":{tagData} d='>'>\" />", tagData);
            FailFirstLast($"<b c=\"ns:{tagData} d='>'>\" />", tagData);
            FailFirstLast($"<b c=':{tagData} \r\n\t d=\">\">' />", tagData);
            FailFirstLast($"<b c='ns:{tagData} \r\n\t d=\">\">' />", tagData);
            FailFirstLast($"<:{tagData}>", tagData);
        }

        var encoding = _encoding;
        var closing = encoding.GetBytes($"<{tagData}>");
        var selfClosing = encoding.GetBytes($"<{tagData} />");

        var data = encoding.GetBytes($"<{tagData}><{tagData} /></{tagData}><{tagData} b=3 /><{tagData}\rc=4></{tagData}>").AsSpan();

        var tag = First(data, tagData, TagEnding.Closing, TagEndings.AnyClosing);
        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(closing), Is.True);
        Assert.That(Last(data, tagData, TagEnding.Closing, TagEndings.ClosingHasNoAttributes), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.SelfClosing, TagEndings.SelfClosing);
        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(selfClosing), Is.True);
        Assert.That(Last(data, tagData, TagEnding.SelfClosing, TagEndings.SelfClosingHasNoAttributes), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.SelfClosingHasAttributes, TagEndings.SelfClosingHasAttributes);
        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(encoding.GetBytes($"<{tagData} b=3 />")), Is.True);
        Assert.That(Last(data, tagData, TagEnding.SelfClosingHasAttributes, TagEndings.SelfClosing), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.ClosingHasAttributes, TagEndings.ClosingHasAttributes);
        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(encoding.GetBytes($"<{tagData}\rc=4>")), Is.True);
        Assert.That(Last(data, tagData, TagEnding.ClosingHasAttributes, TagEndings.AnyClosing), Is.EqualTo(tag));

        tag = First(data, tagData, TagEnding.AttributeStart, TagEndings.HasAttributes);
        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(encoding.GetBytes($"<{tagData} ")), Is.True);

        tag = Last(data, tagData, TagEnding.AttributeStart, TagEndings.HasAttributes);
        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(encoding.GetBytes($"<{tagData}\r")), Is.True);
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

        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(data), Is.True);

        EndingTest(tag, ending);

        Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));
        Assert.That(_finder.First(data, tagData.Name, out var ns, endings), Is.EqualTo(tag));
        Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
        if (tagData.HasNamespace)
        {
            Assert.That(_finder.First(data, tagData.FullName, out ns, endings), Is.EqualTo(tag));
            Assert.That(ns.IsEmpty, Is.True);
        }

        for (int i = 1; i < availableEndings.Length; i++)
        {
            endings = availableEndings[i];
            Assert.That(_finder.First(data, tagData.FullName, endings), Is.EqualTo(tag));
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));
            Assert.That(_finder.First(data, tagData.Name, out ns, endings), Is.EqualTo(tag));
            Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
            if (tagData.HasNamespace)
            {
                Assert.That(_finder.First(data, tagData.FullName, out ns, endings), Is.EqualTo(tag));
                Assert.That(ns.IsEmpty, Is.True);
            }
        }

        foreach (var notAvailableEndings in GetNotAvailableEndings(ending))
        {
            Assert.That(_finder.First(data, tagData.FullName, notAvailableEndings).IsEmpty, Is.True);
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, notAvailableEndings).IsEmpty, Is.True);
            Assert.That(_finder.First(data, tagData.Name, out ns, notAvailableEndings).IsEmpty, Is.True);
            Assert.That(ns.IsEmpty, Is.True);
            if (tagData.HasNamespace)
            {
                Assert.That(_finder.First(data, tagData.FullName, out ns, notAvailableEndings).IsEmpty, Is.True);
                Assert.That(ns.IsEmpty, Is.True);
            }
        }

        if (endingName.IsEmpty)
        {
            Assert.That(_finder.First(data, tagData.FullName, TagEndings.HasNoAttributes), Is.EqualTo(tag));
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasNoAttributes), Is.EqualTo(tag));
            Assert.That(_finder.First(data, tagData.Name, out ns, TagEndings.HasNoAttributes), Is.EqualTo(tag));
            Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
            if (tagData.HasNamespace)
            {
                Assert.That(_finder.First(data, tagData.FullName, out ns, TagEndings.HasNoAttributes), Is.EqualTo(tag));
                Assert.That(ns.IsEmpty, Is.True);
            }
        }
        else
        {
            var tag2 = _finder.First(data, tagData.FullName, TagEndings.HasNoAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.Name));
            Assert.That(data.Slice(tag2.Start, tag2.Length).SequenceEqual(endingName), Is.True);

            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasNoAttributes), Is.EqualTo(tag2));
            Assert.That(_finder.First(data, tagData.Name, out ns, TagEndings.HasNoAttributes), Is.EqualTo(tag2));
            Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
            if (tagData.HasNamespace)
            {
                Assert.That(_finder.First(data, tagData.FullName, out ns, TagEndings.HasNoAttributes), Is.EqualTo(tag2));
                Assert.That(ns.IsEmpty, Is.True);
            }
        }

        //Без атрибутов
        if (attributeStart.IsEmpty)
        {
            Assert.That(_finder.First(data, tagData.FullName, TagEndings.HasAttributes).IsEmpty, Is.True);
            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasAttributes).IsEmpty, Is.True);
            Assert.That(_finder.First(data, tagData.Name, out ns, TagEndings.HasAttributes).IsEmpty, Is.True);
            Assert.That(ns.IsEmpty, Is.True);
            if (tagData.HasNamespace)
            {
                Assert.That(_finder.First(data, tagData.FullName, out ns, TagEndings.HasAttributes).IsEmpty, Is.True);
                Assert.That(ns.IsEmpty, Is.True);
            }
        }
        else
        {
            var tag2 = _finder.First(data, tagData.FullName, TagEndings.HasAttributes);
            Assert.That(tag2.Unended, Is.EqualTo(TagEnding.AttributeStart));
            Assert.That(data.Slice(tag2.Start, tag2.Length).SequenceEqual(attributeStart), Is.True);

            Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, TagEndings.HasAttributes), Is.EqualTo(tag2));
            Assert.That(_finder.First(data, tagData.Name, out ns, TagEndings.HasAttributes), Is.EqualTo(tag2));
            Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
            if (tagData.HasNamespace)
            {
                Assert.That(_finder.First(data, tagData.FullName, out ns, TagEndings.HasAttributes), Is.EqualTo(tag2));
                Assert.That(ns.IsEmpty, Is.True);
            }
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

        Assert.That(data.Slice(tag.Start, tag.Length).SequenceEqual(data), Is.True);

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
            Assert.That(data.Slice(tag2.Start, tag2.Length).SequenceEqual(endingName), Is.True);

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
            Assert.That(data.Slice(tag2.Start, tag2.Length).SequenceEqual(attributeStart), Is.True);

            Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, TagEndings.HasAttributes), Is.EqualTo(tag2));
        }

        return tag;
    }

    private void FailFirstLast(string str, TagData tagData, TagEndings endings = default)
    {
        FailFirstLast(_encoding.GetBytes(str), tagData, endings);
    }

    private void FailFirstLast(ReadOnlySpan<byte> data, TagData tagData, TagEndings endings = default)
    {
        Assert.That(_finder.First(data, tagData.FullName, endings).IsEmpty, Is.True);
        Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings).IsEmpty, Is.True);
        Assert.That(_finder.First(data, tagData.Name, out var ns, endings).IsEmpty, Is.True);
        Assert.That(ns.IsEmpty, Is.True);
        if (tagData.HasNamespace)
        {
            Assert.That(_finder.First(data, tagData.FullName, out ns).IsEmpty, Is.True);
            Assert.That(ns.IsEmpty, Is.True);
        }
        Assert.That(_finder.Last(data, tagData.FullName, endings).IsEmpty, Is.True);
        Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, endings).IsEmpty, Is.True);
        Assert.That(_finder.Last(data, tagData.Name, out ns, endings).IsEmpty, Is.True);
        Assert.That(ns.IsEmpty, Is.True);
        if (tagData.HasNamespace)
        {
            Assert.That(_finder.Last(data, tagData.FullName, out ns).IsEmpty, Is.True);
            Assert.That(ns.IsEmpty, Is.True);
        }
    }

    private Tag First(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending, TagEndings endings)
    {
        var tag = _finder.First(data, tagData.FullName, endings);

        Assert.That(_finder.First(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));
        Assert.That(_finder.First(data, tagData.Name, out var ns, endings), Is.EqualTo(tag));
        Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
        if (tagData.HasNamespace)
        {
            Assert.That(_finder.First(data, tagData.FullName, out ns, endings), Is.EqualTo(tag));
            Assert.That(ns.IsEmpty, Is.True);
        }

        EndingTest(tag, ending);

        return tag;
    }

    private Tag Last(ReadOnlySpan<byte> data, TagData tagData, TagEnding ending, TagEndings endings)
    {
        var tag = _finder.Last(data, tagData.FullName, endings);

        Assert.That(_finder.Last(data, tagData.Name, tagData.Namespace, endings), Is.EqualTo(tag));
        //Assert.That(_finder.Last(data, tagData.Name, out var ns, endings), Is.EqualTo(tag));
        //Assert.That(data.Slice(ns.Start, ns.Length).SequenceEqual(tagData.Namespace), Is.True);
        //if (tagData.HasNamespace)
        //{
        //    Assert.That(_finder.Last(data, tagData.FullName, out ns, endings), Is.EqualTo(tag));
        //    Assert.That(ns.IsEmpty, Is.True);
        //}

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