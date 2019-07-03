import React, { ChangeEvent } from "react";
import { Container } from "../Components/Container";
import { Link } from "react-router-dom";
import { webSocket, WebSocketSubject } from "rxjs/webSocket";
import { Subscription } from "rxjs";
import "./ChannelPage.css";

interface Props {}

interface State {
  message: string;
  messages: string[];
}

export class ChannelPage extends React.Component<Props, State> {
  webSocketSubject?: WebSocketSubject<any>;
  subscription?: Subscription;

  constructor(props: Props) {
    super(props);

    this.state = {
      message: "",
      messages: []
    };
  }

  websocketUrl(path: string) {
    return `${window.location.protocol === "http:" ? "ws" : "wss"}://${
      window.location.host
    }/${path}`;
  }

  componentDidMount() {
    this.webSocketSubject = webSocket(this.websocketUrl("websocket"));
    this.subscription = this.webSocketSubject.subscribe(data => {
      console.log(data);
    });
  }

  componentWillUnmount() {
    if (this.subscription) {
      this.subscription.unsubscribe();
      this.subscription = undefined;
    }
  }

  updateMessage = (event: ChangeEvent<HTMLInputElement>) => {
    this.setState({
      message: event.target.value
    });
  };

  submit = () => {
    if (!this.state.message || !this.webSocketSubject) {
      return;
    }

    this.webSocketSubject.next({
      type: "message",
      payload: {
        message: this.state.message
      }
    });

    this.setState({
      message: "",
      messages: [...this.state.messages, this.state.message]
    });
  };

  render() {
    return (
      <div className="page page--channel">
        <header className="page__header">
          <Container className={["flex__box", "flex__box--vc"]}>
            <h4 className="flex__item">Title</h4>
            <Link className="button button--primary" to="/">
              Exit
            </Link>
          </Container>
        </header>
        <main>
          <Container>
            {this.state.messages.map((message, index) => {
              return <p key={index}>{message}</p>;
            })}
          </Container>
        </main>
        <footer className="page__footer">
          <Container className="flex__box">
            <div className="flex__item">
              <input
                className="form__control"
                type="text"
                placeholder="Input message here"
                value={this.state.message}
                onChange={this.updateMessage}
              />
            </div>
            <button
              onClick={this.submit}
              className="button button--primary button--solid"
            >
              Send
            </button>
          </Container>
        </footer>
      </div>
    );
  }
}
