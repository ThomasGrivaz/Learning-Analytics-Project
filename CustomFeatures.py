#----------------------------------------#
# Function that computes custom features #
#----------------------------------------#
from collections import Counter

def CalculateFeatures(VideoEvents=[], ForumEvents=[]):

    # Initialize features dict
    Features = {}

    # Features for video events
    if len(VideoEvents)>0:

        # Calculate custom features
        # Keys: TimeStamp, EventType, VideoID, CurrentTime, OldTime, NewTime, SeekType, OldSpeed, NewSpeed
        EventTypes = VideoEvents['EventType']
        TimeStamps = VideoEvents['TimeStamp']
        print(VideoEvents)
        TimeStampDiffs = [x[0]-x[1] for x in zip(TimeStamps[1:],TimeStamps[:-1])]
        DurationOfVideoActivity = TimeStamps[-1] - TimeStamps[0]
        AverageVideoTimeDiffs = sum(TimeStampDiffs)/max(1,len(TimeStampDiffs))
        NumberVideoWatched = len(set(VideoEvents['VideoID']))
        NumberOfVideoInteractions = EventTypes.count('Video.Seek')+EventTypes.count('Video.Pause')
        NumberOfSlowPlay = sum(newspeed < 1 for newspeed in VideoEvents['NewSpeed'] if newspeed is not None)
        LastVideoEvent = TimeStamps[-1]


        #temp_list = list(EventTypes.values())[0]
        load_indexes = [i for i, j in enumerate(EventTypes) if j == 'Video.Load']
        IDs = VideoEvents['VideoID']
        video_loaded = [IDs[i] for i in load_indexes]
        RewatchingScore = sum(v for v in Counter(video_loaded).values() if v > 1)

        # Append features to dictionary
        Features.update({
            'DurationOfVideoActivity': DurationOfVideoActivity,
            'AverageVideoTimeDiffs': AverageVideoTimeDiffs,
            'NumberVideoWatched': NumberVideoWatched,
            'NumberOfVideoInteractions': NumberOfVideoInteractions,
            'NumberOfSlowPlay': NumberOfSlowPlay,
            'RewatchingScore': RewatchingScore,
            'LastVideoEvent': LastVideoEvent
        })

    # Features for forum events
    if len(ForumEvents)>0:

        # Calculate custom features
        # Keys: TimeStamp, EventType, PostType, PostID, PostLength
        EventTypes = ForumEvents['EventType']
        NumberOfThreadViews = EventTypes.count('Forum.Thread.View')
        NumberOfPosts = EventTypes.count('Forum.Thread.PostOn')+EventTypes.count('Forum.Post.CommentOn')
        NumberOfThreadCreated = EventTypes.count('Forum.Thread.Launch')
        NumberOfUpvotes = EventTypes.count('Forum.Post.Upvote')+EventTypes.count('Forum.Comment.Upvote')

        TimeStamps = ForumEvents['TimeStamp']
        # TimeStampDiffs = [x[0] - x[1] for x in zip(TimeStamps[1:], TimeStamps[:-1])]
        TimeSpentOnForum = TimeStamps[-1] - TimeStamps[0]
        LastForumEvent = TimeStamps[-1]

        # Append features to dictionary
        Features.update({
            'NumberOfThreadViews': NumberOfThreadViews,
            'NumberOfPosts': NumberOfPosts,
            'NumberOfThreadCreated': NumberOfThreadCreated,
            'NumberOfUpvotes': NumberOfUpvotes,
            'TimeSpentOnForum': TimeSpentOnForum,
            'LastForumEvent': LastForumEvent
        })
    # video + forum events combined (e.g. n-grams)
    if len(ForumEvents)>0 and len(VideoEvents)>0:
        pass

    return Features