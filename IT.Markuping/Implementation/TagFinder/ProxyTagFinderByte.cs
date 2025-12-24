using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace IT.Markuping.Implementation;

public class ProxyTagFinderByte<T> : ITagFinder<byte> where T : unmanaged
{
    private readonly ITagFinder<T> _proxy;

    private static int Size => Unsafe.SizeOf<T>();

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
    {
        throw new NotImplementedException();
    }

    public Tags FirstPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes)
    {
        throw new NotImplementedException();
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes, out Range ns)
    {
        throw new NotImplementedException();
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes, ReadOnlySpan<byte> ns)
    {
        throw new NotImplementedException();
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out int nodes)
    {
        throw new NotImplementedException();
    }

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns, TagEndings endings = TagEndings.AnyClosing)
    {
        throw new NotImplementedException();
    }

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = TagEndings.AnyClosing)
    {
        throw new NotImplementedException();
    }

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = TagEndings.AnyClosing)
    {
        throw new NotImplementedException();
    }

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = TagEndings.AnyClosing)
    {
        throw new NotImplementedException();
    }

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = TagEndings.AnyClosing)
    {
        throw new NotImplementedException();
    }

    public TagClosing FirstClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
    {
        throw new NotImplementedException();
    }

    public TagClosing FirstClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
    {
        throw new NotImplementedException();
    }

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns)
    {
        throw new NotImplementedException();
    }

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
    {
        throw new NotImplementedException();
    }

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
    {
        throw new NotImplementedException();
    }

    #endregion ITagFinder

    private static ReadOnlySpan<T> Cast(ReadOnlySpan<byte> span)
        => MemoryMarshal.Cast<byte, T>(span);
}