using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Order;
using IT.Markuping.Extensions;
using IT.Markuping.Implementation;
using IT.Markuping.Interfaces;
using System.Text;
using System.Xml;

namespace IT.Markuping.Benchmarks;

[MemoryDiagnoser]
[MinColumn, MaxColumn]
[Orderer(SummaryOrderPolicy.FastestToSlowest, MethodOrderPolicy.Declared)]
internal class TagFinderBenchmark
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

    private int _count;
    private Data _utf8;
    private Data _utf16;
    private Data _utf16BE;
    private Data _utf32;
    private Data _utf32BE;

    private readonly ProxyTagFinderByte<char> Utf16 = TagFinderChar.Utf16.AsProxy();
    private readonly ProxyTagFinderByte<char> Utf16BE = TagFinderChar.Utf16BE.AsProxy();
    private readonly ProxyTagFinderByte<int> Utf32 = TagFinderInt32.Utf32.AsProxy();
    private readonly ProxyTagFinderByte<int> Utf32BE = TagFinderInt32.Utf32BE.AsProxy();

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
        var doc = new XmlDocument();
        doc.LoadXml(data);

        _utf8 = new(Encoding.UTF8, data, name);
        _utf16 = new(Encoding.Unicode, data, name);
        _utf16BE = new(Encoding.BigEndianUnicode, data, name);
        _utf32 = new(Encoding.UTF32, data, name);
        _utf32BE = new(Encoding.GetEncoding(12001), data, name);
    }

    [Benchmark]
    public Tags Utf8() => FirstPair(TagFinders.Utf8, _utf8);

    [Benchmark]
    public Tags Utf16_Complex() => FirstPair(TagFinders.Utf16, _utf16);

    [Benchmark]
    public Tags Utf16_Proxy() => FirstPair(Utf16, _utf16);

    [Benchmark]
    public Tags Utf16BE_Complex() => FirstPair(TagFinders.Utf16BE, _utf16BE);

    [Benchmark]
    public Tags Utf16BE_Proxy() => FirstPair(Utf16BE, _utf16BE);

    [Benchmark]
    public Tags Utf32_Complex() => FirstPair(TagFinders.Utf32, _utf32);

    [Benchmark]
    public Tags Utf32_Proxy() => FirstPair(Utf32, _utf32);

    [Benchmark]
    public Tags Utf32BE_Complex() => FirstPair(TagFinders.Utf32BE, _utf32BE);

    [Benchmark]
    public Tags Utf32BE_Proxy() => FirstPair(Utf32BE, _utf32BE);

    private Tags FirstPair(ITagFinder<byte> finder, Data data)
    {
        var tags = finder.FirstPair(data._data, data._name, out var nodes);

        if (!data._data.AsSpan()[tags.Outer].SequenceEqual(data._data) || nodes != _count)
            throw new InvalidOperationException();

        return tags;
    }

    public void Test()
    {
        GlobalSetup();

        Utf8();
        Utf16_Complex();
        Utf16_Proxy();
        Utf16BE_Complex();
        Utf16BE_Proxy();
        Utf32_Complex();
        Utf32_Proxy();
        Utf32BE_Complex();
        Utf32BE_Proxy();
    }
}