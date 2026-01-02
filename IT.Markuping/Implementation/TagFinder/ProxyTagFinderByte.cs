using IT.Markuping.Interfaces;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace IT.Markuping.Implementation;

public class ProxyTagFinderByte<T> : IMarkupFinder<byte> where T : unmanaged
{
    private static readonly int Size = Unsafe.SizeOf<T>();

    private readonly IMarkupFinder<T> _proxy;

    static ProxyTagFinderByte()
    {
        if (typeof(T) == typeof(byte))
            throw new ArgumentException("T is byte", nameof(T));
    }

    public ProxyTagFinderByte(IMarkupFinder<T> proxy)
    {
        _proxy = proxy ?? throw new ArgumentNullException(nameof(proxy));
    }

    #region IMarkupFinder

    public Tags FirstTags(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out TagNS ns, out int nodes)
    {
        var tags = _proxy.FirstTags(Cast(data), Cast(name), out ns, out nodes);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tags.MultipleOffset(size);
    }

    public Tags FirstTags(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, out int nodes)
        => _proxy.FirstTags(Cast(data), Cast(name), Cast(ns), out nodes).MultipleOffset(Size);

    public Tags FirstTags(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes)
        => _proxy.FirstTags(Cast(data), Cast(name), out nodes).MultipleOffset(Size);

    public Tags LastTags(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out TagNS ns, out int nodes)
    {
        var tags = _proxy.LastTags(Cast(data), Cast(name), out ns, out nodes);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tags.MultipleOffset(size);
    }

    public Tags LastTags(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, out int nodes)
        => _proxy.LastTags(Cast(data), Cast(name), Cast(ns), out nodes).MultipleOffset(Size);

    public Tags LastTags(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes)
        => _proxy.LastTags(Cast(data), Cast(name), out nodes).MultipleOffset(Size);

    public Tag FirstTag(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out TagNS ns, TagEndings endings = default)
    {
        var tag = _proxy.FirstTag(Cast(data), Cast(name), out ns, endings);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tag.MultipleOffset(size);
    }

    public Tag FirstTag(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = default)
        => _proxy.FirstTag(Cast(data), Cast(name), Cast(ns), endings).MultipleOffset(Size);

    public Tag FirstTag(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = default)
        => _proxy.FirstTag(Cast(data), Cast(name), endings).MultipleOffset(Size);

    public Tag LastTag(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out TagNS ns, TagEndings endings = default)
    {
        var tag = _proxy.LastTag(Cast(data), Cast(name), out ns, endings);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tag.MultipleOffset(size);
    }

    public Tag LastTag(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = default)
        => _proxy.LastTag(Cast(data), Cast(name), Cast(ns), endings).MultipleOffset(Size);

    public Tag LastTag(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = default)
        => _proxy.LastTag(Cast(data), Cast(name), endings).MultipleOffset(Size);

    public TagClosing FirstTagClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
        => _proxy.FirstTagClosing(Cast(data), Cast(name), Cast(ns)).MultipleOffset(Size);

    public TagClosing FirstTagClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
        => _proxy.FirstTagClosing(Cast(data), Cast(name)).MultipleOffset(Size);

    public TagClosing LastTagClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out TagNS ns)
    {
        var tag = _proxy.LastTagClosing(Cast(data), Cast(name), out ns);

        var size = Size;

        ns = ns.MultipleOffset(size);

        return tag.MultipleOffset(size);
    }

    public TagClosing LastTagClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
        => _proxy.LastTagClosing(Cast(data), Cast(name), Cast(ns)).MultipleOffset(Size);

    public TagClosing LastTagClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
        => _proxy.LastTagClosing(Cast(data), Cast(name)).MultipleOffset(Size);

    #endregion IMarkupFinder

    private static ReadOnlySpan<T> Cast(ReadOnlySpan<byte> span)
    {
        if (span.Length == 0) return default;

        return MemoryMarshal.Cast<byte, T>(span);
        //Debug.Assert(Size == Unsafe.SizeOf<T>());

        //TODO: проверить на остаток от деления
        //var length = span.Length / Unsafe.SizeOf<T>();
        //return MemoryMarshal.CreateReadOnlySpan(ref Unsafe.As<byte, T>(ref MemoryMarshal.GetReference(span)), length);
    }
}