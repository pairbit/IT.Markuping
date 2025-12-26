namespace IT.Markuping.Internal;

internal readonly struct StartEnd
{
    internal readonly int _start;
    internal readonly int _end;

    public StartEnd(int start, int end)
    {
        _start = start;
        _end = end;
    }
}