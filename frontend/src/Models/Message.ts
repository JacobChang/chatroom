type TargetType = 'channel'

export interface ErrorMsg {
  type: 'error',
  payload: {
    code: number
  }
}

export interface PingMsg {
  type: 'channel.ping',
  target: {
    type: TargetType,
    id: string,
  }
}

export interface PongMsg {
  type: 'channel.pong',
  target: {
    type: TargetType,
    id: string,
  }
}

export interface JoinMsg {
  type: 'channel.join',
  target: {
    type: TargetType,
    id: string,
  }
}

export interface ChannelInfoMsg {
  type: 'channel.info',
  target: {
    type: TargetType,
    id: string,
  },
  payload: {
    title: string,
  }
}

export interface LeaveMsg {
  type: 'channel.leave',
  target: {
    type: TargetType,
    id: string,
  }
}

export interface LeftMsg {
  type: 'channel.left',
  target: {
    type: TargetType,
    id: string,
  }
}

export interface ChatMsg {
  type: 'channel.chat',
  target: {
    type: TargetType,
    id: string,
  },
  payload: {
    content: string
  }
}

export type Message =
  | PingMsg
  | PongMsg
  | JoinMsg
  | ChannelInfoMsg
  | LeaveMsg
  | LeftMsg
  | ChatMsg
