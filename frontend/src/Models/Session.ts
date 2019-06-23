import React from "react";
import { ajax } from "rxjs/ajax";
import { BehaviorSubject } from 'rxjs';

export interface ISession {
  verified: boolean,
  nickname: string,
  avatar: string,
}

export const defaultSession = {
  verified: false,
  nickname: '',
  avatar: ''
}

export class Session {
  subject: BehaviorSubject<ISession> = new BehaviorSubject(defaultSession);

  sync() {
    ajax('/api/v1/session').subscribe(result => {
      let session = {
        verified: true,
        nickname: '',
        avatar: '',
      };
      this.subject.next(session);
    })
  }
}

export const session = new Session();

export const SessionContext: React.Context<ISession> = React.createContext(
  defaultSession
);