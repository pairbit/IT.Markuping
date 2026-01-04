using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Order;
using IT.Markuping.Interfaces;
using System;
using System.Text;
using System.Xml;

namespace IT.Markuping.Benchmarks;

[MemoryDiagnoser]
[MinColumn, MaxColumn]
[Orderer(SummaryOrderPolicy.FastestToSlowest, MethodOrderPolicy.Declared)]
public class MarkupFinderBenchmark
{
    public readonly struct Data
    {
        internal readonly byte[] _data = null!;
        internal readonly byte[] _name = null!;

        public Data(Encoding encoding, string data, string name)
        {
            _data = encoding.GetBytes(data);
            _name = encoding.GetBytes(name);
        }
    }

    public readonly struct Map
    {
        internal readonly byte _b1, _b2, _b3, _b4;
        internal readonly bool[] _map;

        public Map(byte b1, byte b2, byte b3, byte b4)
        {
            _b1 = b1;
            _b2 = b2;
            _b3 = b3;
            _b4 = b4;
            _map = new bool[256];
            _map[_b1] = true;
            _map[_b2] = true;
            _map[_b3] = true;
            _map[_b4] = true;
        }

        public bool IsTrueAuto(byte index) => _map[index];

        public bool IsTrueManual4(byte index) =>
            index == _b1 || index == _b2 || index == _b3 || index == _b4;

        public bool IsTrueManual2(byte index) =>
            index == _b1 || index == _b2;
    }

    private byte _by;

    private int _count;
    private Map _map;
    private Data _utf8;
    private Data _utf16;
    private Data _utf16BE;
    private Data _utf32;
    private Data _utf32BE;

    [GlobalSetup]
    public void GlobalSetup()
    {
        var name = "ds:Signature";
        var data = @$"<{name} xmlns:ds=""http://www.w3.org/2000/09/xmldsig#"">
    <{name}></{name}>
    <{name}>
        <{name}>
            <{name}></{name}>
            <{name}></{name}>
        </{name}>
        <{name}>
            <{name}></{name}>
            <{name}></{name}>
            <{name}></{name}>
        </{name}>
    </{name}>
    <{name}>
        <{name}>
            <{name}></{name}>
        </{name}>
        <{name}></{name}>
    </{name}>
</{name}>";
        _count = 13;
        _by = 102;
        _map = new(12, 23, 64, 155);
        var doc = new XmlDocument();
        doc.LoadXml(data);

        _utf8 = new(Encoding.UTF8, data, name);
        _utf16 = new(Encoding.Unicode, data, name);
        _utf16BE = new(Encoding.BigEndianUnicode, data, name);
        _utf32 = new(Encoding.UTF32, data, name);
        _utf32BE = new(Encoding.GetEncoding(12001), data, name);
    }

    [Benchmark]
    public bool IsTrueManual2() => _map.IsTrueManual2(_by);

    [Benchmark]
    public bool IsTrueManual4() => _map.IsTrueManual4(_by);

    [Benchmark]
    public bool IsTrueAuto() => _map.IsTrueAuto(_by);

    [Benchmark]
    public Tags Utf8() => FirstTags(MarkupFinders.Utf8, _utf8);

    [Benchmark]
    public Tags Utf8_OtherSpaces() => FirstTags(MarkupFinders.OtherSpaces.Utf8, _utf8);

    [Benchmark]
    public Tags Europa() => FirstTags(MarkupFinders.Europa, _utf8);

    [Benchmark]
    public Tags Europa_OtherSpace() => FirstTags(MarkupFinders.OtherSpaces.Europa, _utf8);

    [Benchmark]
    public Tags Utf16() => FirstTags(MarkupFinders.Utf16, _utf16);

    [Benchmark]
    public Tags Utf16BE() => FirstTags(MarkupFinders.Utf16BE, _utf16BE);

    [Benchmark]
    public Tags Utf32() => FirstTags(MarkupFinders.Utf32, _utf32);

    [Benchmark]
    public Tags Utf32BE() => FirstTags(MarkupFinders.Utf32BE, _utf32BE);

    [Benchmark]
    public Tags Utf16_Complex() => FirstTags(MarkupFinders.Complex.Utf16, _utf16);

    [Benchmark]
    public Tags Utf16BE_Complex() => FirstTags(MarkupFinders.Complex.Utf16BE, _utf16BE);

    [Benchmark]
    public Tags Utf32_Complex() => FirstTags(MarkupFinders.Complex.Utf32, _utf32);

    [Benchmark]
    public Tags Utf32BE_Complex() => FirstTags(MarkupFinders.Complex.Utf32BE, _utf32BE);

    private Tags FirstTags(IMarkupFinder<byte> finder, Data data)
    {
        var tags = finder.FirstTags(data._data, data._name, out var nodes);

        if (!data._data.AsSpan().Slice(tags.Start, tags.Length).SequenceEqual(data._data) || nodes != _count)
            throw new InvalidOperationException();

        return tags;
    }

    public void Test()
    {
        GlobalSetup();

        Utf8();
        Utf8_OtherSpaces();
        Europa();
        Europa_OtherSpace();
        Utf16();
        Utf16BE();
        Utf32();
        Utf32BE();
        Utf16_Complex();
        Utf16BE_Complex();
        Utf32_Complex();
        Utf32BE_Complex();

        if (IsTrueManual2() != IsTrueAuto() || IsTrueManual4() != IsTrueAuto())
            throw new InvalidOperationException();
    }
}