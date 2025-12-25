using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Order;
using IT.Markuping.Implementation;
using System;
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

    [GlobalSetup]
    public void GlobalSetup()
    {
        var name = "ds:Signature";
        var data = @$"
<{name} xmlns:ds=""http://www.w3.org/2000/09/xmldsig#"">
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
</{name}>
";
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
    public Tags Utf8() 
        => TagFinders.Utf8.FirstPair(_utf8._data, _utf8._name, out var nodes);

    public void Test()
    {
        GlobalSetup();


    }
}