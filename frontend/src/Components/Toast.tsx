import React from "react";
import "./Toast.css";
import classNames from "classnames";
import { ClassValue } from "classnames/types";

export interface IToast {
  message: string;
  duration?: number;
}

export interface Props {
  toast: IToast;
  onEnd?: (toast: IToast) => void;
  className?: ClassValue[] | ClassValue;
}

export interface State {
  isEnded: boolean;
}

export class Toast extends React.PureComponent<Props, State> {
  timer: any;

  constructor(props: Props) {
    super(props);

    this.state = {
      isEnded: false
    };
  }

  componentDidMount() {
    const { toast, onEnd } = this.props;
    const { duration } = toast;

    if (duration) {
      this.timer = setTimeout(() => {
        this.setState({
          isEnded: true
        });
        this.timer = null;
        if (onEnd) {
          onEnd(toast);
        }
      }, duration);
    }
  }

  componentWillUnmount() {
    if (this.timer) {
      clearTimeout(this.timer);
    }
  }

  render() {
    const { isEnded } = this.state;
    if (isEnded) {
      return null;
    }

    const { toast, className } = this.props;

    return (
      <div className={classNames("toast", className)}>
        <p>{toast.message}</p>
      </div>
    );
  }
}
