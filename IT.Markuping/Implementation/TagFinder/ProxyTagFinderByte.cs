using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace IT.Markuping.Implementation;

public class ProxyTagFinderByte<T> : ITagFinder<byte> where T : unmanaged
{
    private static readonly int Size = Unsafe.SizeOf<T>();

    private readonly ITagFinder<T> _proxy;

    static ProxyTagFinderByte()
    {
        if (typeof(T) == typeof(byte))
            throw new ArgumentException("T is byte", nameof(T));
    }

    public ProxyTagFinderByte(ITagFinder<T> proxy)
    {
        _proxy = proxy ?? throw new ArgumentNullException(nameof(proxy));
    }

    #region ITagFinder

    public Tags FirstPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes, out Range ns)
    {
        var tags = _proxy.FirstPair(Cast(data), Cast(name), out nodes, out ns);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tags.MultipleOffset(size);
    }

    public Tags FirstPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes, ReadOnlySpan<byte> ns)
        => _proxy.FirstPair(Cast(data), Cast(name), out nodes, Cast(ns)).MultipleOffset(Size);

    public Tags FirstPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes)
        => _proxy.FirstPair(Cast(data), Cast(name), out nodes).MultipleOffset(Size);

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes, out Range ns)
    {
        var tags = _proxy.LastPair(Cast(data), Cast(name), out nodes, out ns);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tags.MultipleOffset(size);
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes, ReadOnlySpan<byte> ns)
        => _proxy.LastPair(Cast(data), Cast(name), out nodes, Cast(ns)).MultipleOffset(Size);

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes)
        => _proxy.LastPair(Cast(data), Cast(name), out nodes).MultipleOffset(Size);

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns, TagEndings endings = TagEndings.AnyClosing)
    {
        var tag = _proxy.First(Cast(data), Cast(name), out ns, endings);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tag.MultipleOffset(size);
    }

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = TagEndings.AnyClosing)
        => _proxy.First(Cast(data), Cast(name), Cast(ns), endings).MultipleOffset(Size);

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = TagEndings.AnyClosing)
        => _proxy.First(Cast(data), Cast(name), endings).MultipleOffset(Size);

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = TagEndings.AnyClosing)
        => _proxy.Last(Cast(data), Cast(name), Cast(ns), endings).MultipleOffset(Size);

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = TagEndings.AnyClosing)
        => _proxy.Last(Cast(data), Cast(name), endings).MultipleOffset(Size);

    public TagClosing FirstClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
        => _proxy.FirstClosing(Cast(data), Cast(name), Cast(ns)).MultipleOffset(Size);

    public TagClosing FirstClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
        => _proxy.FirstClosing(Cast(data), Cast(name)).MultipleOffset(Size);

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns)
    {
        var tag = _proxy.LastClosing(Cast(data), Cast(name), out ns);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tag.MultipleOffset(size);
    }

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
        => _proxy.LastClosing(Cast(data), Cast(name), Cast(ns)).MultipleOffset(Size);

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
        => _proxy.LastClosing(Cast(data), Cast(name)).MultipleOffset(Size);

    #endregion ITagFinder

    private static ReadOnlySpan<T> Cast(ReadOnlySpan<byte> span)
        => MemoryMarshal.Cast<byte, T>(span);
}