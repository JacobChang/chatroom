import React from "react";
import { Container } from "../Components/Container";
import { Link } from "react-router-dom";
import { webSocket, WebSocketSubject } from "rxjs/webSocket";
import { Subscription } from "rxjs";

interface Props {}

interface State {
  title: string;
  duration: number;
  max_members: number;
}

export class ChannelPage extends React.Component<Props, State> {
  subscription?: Subscription;

  websocketUrl(path: string) {
    return `${window.location.protocol === "http:" ? "ws" : "wss"}://${
      window.location.host
    }/${path}`;
  }

  componentDidMount() {
    let webSocketSubject = webSocket(this.websocketUrl("websocket"));
    this.subscription = webSocketSubject.subscribe(data => {
      console.log(data);
    });
    webSocketSubject.next({
      type: "join"
    });
  }

  componentWillUnmount() {
    if (this.subscription) {
      this.subscription.unsubscribe();
      this.subscription = undefined;
    }
  }

  render() {
    return (
      <div className="page page--channel">
        <header>
          <Container className={["flex__box", "flex__box--vc"]}>
            <h4 className="flex__item">
              <Link to="/">Back to Home</Link>
            </h4>
          </Container>
        </header>
        <section>
          <Container>
            <p>Channel Page</p>
          </Container>
        </section>
      </div>
    );
  }
}
